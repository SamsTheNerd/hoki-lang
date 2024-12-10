module InterpLayer.InterpMap where
import qualified CoreLang.CoreSorts as Core
import qualified Hoki.FrontSorts as Hoki
import Data.Map ( empty, Map, toList, member, (!), insert )
import Data.List ( foldl' )
import Control.Exception
import CoreLang.CoreLoader (LProg(..))

data InterpException = UnhandledType | Unimplemented
    deriving (Show)
instance Exception InterpException

type InterpMap = Map Hoki.Ident (Core.Expr,Core.Type)
-- Core.TypeCons "Bool" [] [DataCons "True" [], DataCons "False" []]
-- Core.TypeCons "String" [] [DataCons "StrCons" ["Char", "List Char"], DataCons "StrEmpty" []]
hokiToCoreLit :: Hoki.Literal -> Core.Literal
hokiToCoreLit (Hoki.LInt x) = Core.LInt (fromInteger x)
hokiToCoreLit (Hoki.LDec x) = Core.LDouble x
hokiToCoreLit (Hoki.LChar x) = Core.LChar x
hokiToCoreLit _ = undefined

hokiToCoreArg :: Hoki.Args -> Core.Expr
hokiToCoreArg (Hoki.Avar name) = Core.EVar name
hokiToCoreArg (Hoki.Alit literal) = Core.ELit (hokiToCoreLit literal)

hokiToCoreExpr :: LProg -> Hoki.Expr -> Core.Expr
hokiToCoreExpr _ (Hoki.Evar name) = Core.EVar name
hokiToCoreExpr _ (Hoki.Elit (Hoki.LBool bool)) = Core.ECons (show bool) []
hokiToCoreExpr _ (Hoki.Elit (Hoki.LStr _)) = Core.ECons ("StrEmpty") []
-- hokiToCoreExpr _ (Hoki.Elit (Hoki.LStr (x:xs))) = Core.ECons ("StrCons") [x,xs]
--     where
--         foldStr f u xs = foldStr 
hokiToCoreExpr _ (Hoki.Elit literal) = Core.ELit (hokiToCoreLit literal)
hokiToCoreExpr (LProg tenv _ _ venv) (Hoki.Eapp func args) = let
    args' = map hokiToCoreArg args
    func' = case member func venv of
        False -> throw Unimplemented
        True -> venv ! func
    in case args' of
        [] -> throw Unimplemented
        (x:xs) -> foldl' Core.EApp (Core.EApp func' x) xs
hokiToCoreExpr lprog@(LProg tenv a b venv) (Hoki.Eabb name args exprs typesig) = let
    exprscl = map (hokiToCoreExpr lprog) exprs
    expr = case args of
        [] -> throw Unimplemented
        xs -> foldr Core.ELambda (Core.ELambda (last xs) (head exprscl)) (init xs)
    -- tenv' = insert name ()
    tenv' = tenv
    venv' = insert name expr venv
    lprog' = LProg tenv' a b venv'
    in expr
hokiToCoreExpr _ (Hoki.Econs x) = throw Unimplemented