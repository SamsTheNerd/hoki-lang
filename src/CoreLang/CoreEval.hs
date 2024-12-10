{-# LANGUAGE LambdaCase #-}
module CoreLang.CoreEval where
import Control.Monad.State
import Data.Map ( empty, singleton, Map, toList )
import CoreLang.CoreSorts
import CoreLang.Runad
import Control.Monad.Trans.Maybe (MaybeT (..), mapMaybeT)
import qualified Data.Bifunctor

type Subst = Map Ident Expr -- subst maps for laziness

fullEval :: Expr -> IO (Either EError Expr)
fullEval expr = fst <$> runRunAd (eval expr) (RunadSTIn empty empty 0)

-- eval lazily ?
eval :: Expr -> Runad Expr
eval e1@(EVar vid) = do -- immediately substitute it
    me2 <- lookupVar vid
    case me2 of
        (Just (EVar vid')) | vid == vid' -> return e1
        (Just e2) -> eval e2
        Nothing -> return e1
eval e1@(ELit _) = return e1
eval e1@(ELambda _ _) = return e1 -- lazy
eval e1@(EPrimOp _) = return e1 -- lazy
eval e1@(ECons _ _) = return e1 -- lazy

eval (EApp (ELambda argId body) arg) = extendVarEnv argId arg (eval (varSub argId arg body))
eval (EApp (EPrimOp (PrimOp pop _ _)) arg) = pop arg
eval (EApp func arg) = do
    eF <- eval func
    eval (EApp eF arg)
eval e1@(ECase _ []) = runadErr $ "Non-exhaustive patterns in: " ++ show e1
eval (ECase inp ((p, pe):ps)) = do
    res <- runMaybeT $ patternMatch p inp
    case res of
        (Just ve) -> extendVarEnv' ve (eval (varSubs ve pe))
        Nothing -> eval (ECase inp ps)

-- `varSub v new in` substitutes the given variable with the new expression in the other expression
varSub :: Ident -> Expr -> Expr -> Expr
varSub v newExpr inExp@(EVar vid) = if vid == v then newExpr else inExp
varSub v newExpr (ELambda arg body) = ELambda arg (varSub v newExpr body)
varSub v newExpr (EApp func arg) = EApp (varSub v newExpr func) (varSub v newExpr arg)
varSub v newExpr (ECons name es) = ECons name (map (varSub v newExpr) es)
varSub v newExpr (ECase inp ps) = ECase (varSub v newExpr inp)
    (map (Data.Bifunctor.second (varSub v newExpr)) ps)
varSub _ _ inExp = inExp

varSubs :: VarEnv -> Expr -> Expr
varSubs venv expr = foldr (uncurry varSub) expr (toList venv)

-- checks if an expression matches a pattern, and if so returns the bindings
-- TODO: error check on all the binds that no var is being double bound here?
patternMatch :: Pattern -> Expr -> MaybeT Runad VarEnv
patternMatch PAny _ = (MaybeT . return . Just) empty
patternMatch (PLit (ELit p1)) (ELit p2) = MaybeT . return $ if p1 == p2 then Just empty else Nothing
patternMatch p@(PLit (ELit _)) e = (lift . eval) e >>= patternMatch p
patternMatch (PLit _) _ = MaybeT $ return Nothing -- come back to this if we work out better equality
patternMatch (PVar v) expr =
    mapMaybeT (>>= \case
            -- if we find a dataconstructor then go do that
            (Just _) -> runMaybeT $ patternMatch (PCons v []) expr
            -- otherwise just bind
            Nothing -> (return . Just . singleton v) expr
        ) (MaybeT $ lookupDC v)

patternMatch (PLabel v p) expr = singleton v expr <$ patternMatch p expr
patternMatch (PCons _ ps) expr = do
    expr' <- (lift . eval) expr -- need to check the expression so eval it
    case expr' of
        (ECons _ fields) -> if length fields /= length ps then MaybeT $ return Nothing
        else foldr (\(p, e) rmvenv -> do
            mvenv <- rmvenv
            (mvenv <>) <$> patternMatch p e
            ) ((MaybeT . return . Just) empty) (reverse $ zip ps fields)
        _ -> MaybeT $ return Nothing