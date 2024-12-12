module InterpLayer.InterpMap where
import qualified CoreLang.CoreSorts as Core
import qualified Hoki.FrontSorts as Hoki
import Data.Map ( empty, Map, toList, member, (!), insert )
import Data.List ( foldl' )
import Control.Exception
import CoreLang.CoreLoader (LProg(..))

data InterpException = Unimplemented
                     deriving (Show)
instance Exception InterpException

hokiToCoreLit :: Hoki.Literal -> Core.Expr
hokiToCoreLit (Hoki.LInt  i) = Core.ECons "NumInt" [Core.ELit $ Core.LInt i']
    where i' = fromInteger i
hokiToCoreLit (Hoki.LDec  d) = Core.ECons "NumDouble" [Core.ELit $ Core.LDouble d]
hokiToCoreLit (Hoki.LChar c) = Core.ELit $ Core.LChar c
hokiToCoreLit (Hoki.LBool b) = Core.ECons (show b) []
hokiToCoreLit (Hoki.LStr  s) = strToCore s
    where
        strToCore :: String -> Core.Expr
        strToCore []     = Core.ECons "Empty" []
        strToCore (x:xs) = Core.ECons "Cons" [Core.ELit (Core.LChar x), strToCore xs]

hokiToCoreArg :: Hoki.Args -> Core.Expr
hokiToCoreArg (Hoki.Avar name) = Core.EVar name
hokiToCoreArg (Hoki.Alit lit)  = hokiToCoreLit lit

hokiToCoreType :: Hoki.Type -> Core.Type
hokiToCoreType (Hoki.Tsig t) = Core.TNamed t
hokiToCoreType (Hoki.Tabb args res) = foldr (Core.TArrow) (Core.TArrow (last args') res') (init args')
    where
        args' = map hokiToCoreType args
        res' = hokiToCoreType res

hokiToCoreExpr :: LProg -> Hoki.Expr -> (LProg,Core.Expr)
hokiToCoreExpr lprog (Hoki.Evar name) = (lprog,Core.EVar name)
hokiToCoreExpr lprog (Hoki.Elit lit) = (lprog,hokiToCoreLit lit)
hokiToCoreExpr lprog (Hoki.Eapp func args) = case args' of
    []     -> (lprog,func')
    (x:xs) -> (lprog,foldl' Core.EApp (Core.EApp func' x) xs)
    where
        func' = Core.EVar func
        args' = map hokiToCoreArg args
hokiToCoreExpr lp@(LProg tenv a b venv) (Hoki.Eabb name args exprs ts) = (lprog',expr)
    where
        exprscl = map (snd . hokiToCoreExpr lp) exprs
        expr = case args of
            [] -> head exprscl
            xs -> foldr Core.ELambda (Core.ELambda (last xs) (head exprscl)) (init xs)
        tenv' = case ts of
            Just t  -> insert name (hokiToCoreType t) tenv
            Nothing tenv
        venv' = insert name expr venv
        lprog' = LProg tenv' a b venv'
hokiToCoreExpr lprog (Hoki.Econs x) = (lprog,Core.ECons x [])
