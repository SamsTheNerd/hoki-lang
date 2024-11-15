module CoreLang.CoreEval where
import Control.Monad.State
import Data.Map
import CoreLang.CoreSorts
import qualified Data.Maybe
import CoreLang.Runad

type Subst = Map Ident Expr -- subst maps for laziness

fullEval :: Expr -> IO (Either Expr EError)
fullEval exp = fst <$> runRunAd (eval exp) (RunadSTIn empty 0)

-- eval lazily ?
eval :: Expr -> Runad Expr
eval e1@(EVar id) = subst e1 -- immediately substitute it
eval e1@(ELit _) = return e1
eval e1@(ELambda arg body) = ELambda arg <$> eval body -- lazy?
-- eval e1@(ELambda arg body) = do
    -- i <- get
    -- modify (+1)
    -- eval (insert arg (EVar (arg ++ show i)) subs) body
eval e1@(EApp func@(ELambda argId body) arg) = do -- prob need to alpha rename around here?
    extendVarEnv argId arg (eval body)
eval e1@(EApp func arg) = do
    eF <- eval func
    eval (EApp eF arg)


-- immediate substitution
subst :: Expr -> Runad Expr
subst e1@(EVar id) = Data.Maybe.fromMaybe e1 <$> lookupVar id
-- subst e1@(ELambda arg body) = subst (delete arg subs) body
-- subst e1@(EApp func arg) = EApp (subst subs func) (subst subs arg)
subst e = return e