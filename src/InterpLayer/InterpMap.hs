module InterpLayer.InterpMap where
import qualified CoreLang.CoreSorts as Core
import qualified Hoki.FrontSorts as Hoki
import Data.Map ( empty, Map, toList, member, (!), insert )
import Data.List ( foldl' )
import Control.Exception
import CoreLang.CoreLoader (LProg(..))

data InterpException = Unreachable
                     | Unimplemented
                     | UnknownFunction Hoki.Ident
                     deriving (Show)
instance Exception InterpException

-- Data constructors for Bool and List
-- Core.TypeCons "Bool" [] [DataCons "True" [], DataCons "False" []]
-- Core.TypeCons "List" [a] [DataCons "Cons" ["a", "List a"], DataCons "Empty" []]

hokiToCoreLit :: Hoki.Literal -> Core.Expr
hokiToCoreLit (Hoki.LInt x) = Core.ECons "NumInt" [Core.ELit $ Core.LInt (fromInteger x)]
hokiToCoreLit (Hoki.LDec x) = Core.ECons "NumDouble" [Core.ELit $ Core.LDouble x]
hokiToCoreLit (Hoki.LChar x) = Core.ELit $ Core.LChar x
hokiToCoreLit _ = throw Unreachable

hokiToCoreArg :: Hoki.Args -> Core.Expr
hokiToCoreArg (Hoki.Avar name) = Core.EVar name
hokiToCoreArg (Hoki.Alit literal) = hokiToCoreLit literal

stringToCore :: String -> Core.Expr
stringToCore [] = Core.ECons "Empty" []
stringToCore (x:xs) = Core.ECons "Cons" [Core.ELit (Core.LChar x), stringToCore xs]

hokiToCoreExpr :: LProg -> Hoki.Expr -> (LProg,Core.Expr)
hokiToCoreExpr lprog (Hoki.Evar name) = (lprog,Core.EVar name)
hokiToCoreExpr lprog (Hoki.Elit (Hoki.LBool bool)) = (lprog,Core.ECons (show bool) [])
hokiToCoreExpr lprog (Hoki.Elit (Hoki.LStr str)) = (lprog,stringToCore str)
hokiToCoreExpr lprog (Hoki.Elit literal) = (lprog,hokiToCoreLit literal)
hokiToCoreExpr lprog@(LProg _ _ _ venv) (Hoki.Eapp func args) = case args' of
    [] -> throw Unimplemented
    (x:xs) -> (lprog,foldl' Core.EApp (Core.EApp (Core.EVar func) x) xs)
    where
        args' = map hokiToCoreArg args
hokiToCoreExpr lp@(LProg tenv a b venv) (Hoki.Eabb name args exprs typesig) = (lprog',expr)
    where
        exprscl = map (snd . hokiToCoreExpr lp) exprs
        expr = case args of
            [] -> throw Unimplemented
            xs -> foldr Core.ELambda (Core.ELambda (last xs) (head exprscl)) (init xs)
        tenv' = tenv
        venv' = insert name expr venv
        lprog' = LProg tenv' a b venv'
hokiToCoreExpr lprog (Hoki.Econs x) = throw Unimplemented
