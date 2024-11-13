module CoreLang.CoreEval where
import Control.Monad.State
import Data.Map
import CoreLang.CoreSorts
import qualified Data.Maybe

type Subst = Map Ident Expr -- subst maps for laziness

fullEval exp = runState (eval empty exp) 0

-- eval lazily ?
eval :: Subst -> Expr -> State Int Expr
eval subs e1@(EVar id) = return $ subst subs e1 -- immediately substitute it
eval subs e1@(ELit _) = return e1
eval subs e1@(ELambda arg body) = ELambda arg <$> eval subs body -- lazy?
-- eval subs e1@(ELambda arg body) = do
--     i <- get
--     modify (+1)
--     eval (insert arg (EVar (arg ++ show i)) subs) body
eval subs e1@(EApp func@(ELambda argId body) arg) = do
    eval (insert argId arg subs) body
    -- fRed <- eval subs func -- reduce the function first to see if it becomes function-like? may need to like, instantiate it here too?
    -- case fRed of 
    --     (ELambda argId lBody) -> eval (insert argId arg subs) lBody
    --     _ -> return fRed
eval subs e1@(EApp func arg) = do
    eF <- eval subs func
    -- eArg <- eval subs arg
    eval subs (EApp eF arg)


-- immediate substitution
subst :: Subst -> Expr -> Expr
subst subs e1@(EVar id) = Data.Maybe.fromMaybe e1 (Data.Map.lookup id subs)
subst subs e1@(ELambda arg body) = subst (delete arg subs) body
subst subs e1@(EApp func arg) = EApp (subst subs func) (subst subs arg)
-- subst subs e = e