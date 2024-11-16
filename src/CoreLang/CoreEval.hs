{-# LANGUAGE LambdaCase #-}
module CoreLang.CoreEval where
import Control.Monad.State
import Data.Map ( empty, singleton, Map, toList )
import CoreLang.CoreSorts
import qualified Data.Maybe
import CoreLang.Runad
import Control.Monad.Trans.Maybe (MaybeT (..), mapMaybeT)
import qualified Data.Bifunctor

type Subst = Map Ident Expr -- subst maps for laziness

fullEval :: Expr -> IO (Either EError Expr)
fullEval exp = fst <$> runRunAd (eval exp) (RunadSTIn empty empty 0)

-- eval lazily ?
eval :: Expr -> Runad Expr
-- eval e1@(EVar id) = subst e1
-- eval e1@(EVar id) = ((\m -> maybe (return e1) eval m) <$> lookupVar id) -- immediately substitute it
eval e1@(EVar id) = do -- immediately substitute it
    me2 <- lookupVar id
    case me2 of
        (Just (EVar id')) | id == id' -> return e1
        (Just e2) -> eval e2
        Nothing -> return e1
    -- if me2 == e1 then return me2 else eval me2
eval e1@(ELit _) = return e1
-- eval e1@(ELambda arg body) = ELambda arg <$> eval body -- lazy?
eval e1@(ELambda arg body) = return e1 -- lazy?
eval e1@(EPrimOp _) = return e1 -- lazy?
eval e1@(ECons _ _) = return e1 -- lazy?
-- eval e1@(ELambda arg body) = do
    -- i <- get
    -- modify (+1)
    -- eval (insert arg (EVar (arg ++ show i)) subs) body
eval e1@(EApp func@(ELambda argId body) arg) = do -- prob need to alpha rename around here?
    -- refId <- gecrFresh argId
    extendVarEnv argId arg (eval (varSub argId arg body))
eval e1@(EApp func@(EPrimOp (PrimOp pop)) arg) = pop arg
eval e1@(EApp func arg) = do
    eF <- eval func
    eval (EApp eF arg)
eval e1@(ECase inp []) = runadErr $ "Non-exhaustive patterns in: " ++ show e1
eval e1@(ECase inp ((p, pe):ps)) = do
    res <- runMaybeT $ patternMatch p inp
    case res of
        (Just ve) -> extendVarEnv' ve (eval (varSubs ve pe))
        Nothing -> eval (ECase inp ps)

-- immediate substitution
-- subst :: Expr -> Runad Expr
-- subst e1@(EVar id) = Data.Maybe.fromMaybe e1 <$> lookupVar id
-- subst e1@(ELambda arg body) = subst body
-- -- subst e1@(EApp func arg) = do
-- --     fsub <- subst func
-- --     asub <- subst arg
-- --     return $ EApp fsub arg
-- -- subst e = return e

-- `varSub v new in` substitutes the given variable with the new expression in the other expression
varSub :: Ident -> Expr -> Expr -> Expr
varSub v newExpr inExp@(EVar id) = if id == v then newExpr else inExp
varSub v newExpr inExp@(ELambda arg body) = ELambda arg (varSub v newExpr body)
varSub v newExpr inExp@(EApp func arg) = EApp (varSub v newExpr func) (varSub v newExpr arg)
varSub v newExpr inExp@(ECons name es) = ECons name (map (varSub v newExpr) es)
varSub v newExpr inExp@(ECase inp ps) = ECase (varSub v newExpr inp)
    (map (Data.Bifunctor.second (varSub v newExpr)) ps)
varSub v newExpr inExp = inExp

varSubs :: VarEnv -> Expr -> Expr
varSubs venv exp = foldr (uncurry varSub) exp (toList venv)

-- checks if an expression matches a pattern, and if so returns the bindings
-- TODO: error check on all the binds that no var is being double bound here?
patternMatch :: Pattern -> Expr -> MaybeT Runad VarEnv
patternMatch PAny _ = (MaybeT . return . Just) empty
patternMatch (PLit (ELit p1)) (ELit p2) = MaybeT . return $ if p1 == p2 then Just empty else Nothing
patternMatch p@(PLit (ELit p1)) e = (lift . eval) e >>= patternMatch p
patternMatch (PLit _) _ = MaybeT $ return Nothing -- come back to this if we work out better equality
patternMatch (PVar v) expr =
    mapMaybeT (>>= \case
            -- if we find a dataconstructor then go do that
            (Just dc) -> runMaybeT $ patternMatch (PCons v []) expr
            -- otherwise just bind
            Nothing -> (return . Just . singleton v) expr
        ) (MaybeT $ lookupDC v)

patternMatch (PLabel v p) expr = singleton v expr <$ patternMatch p expr
patternMatch (PCons id ps) expr = do
    expr' <- (lift . eval) expr -- need to check the expression so eval it
    case expr' of
        (ECons dcid fields) -> if length fields /= length ps then MaybeT $ return Nothing
        else foldr (\(p, e) rmvenv -> do
            mvenv <- rmvenv
            (mvenv <>) <$> patternMatch p e
            ) ((MaybeT . return . Just) empty) (reverse $ zip ps fields)
        _ -> MaybeT $ return Nothing