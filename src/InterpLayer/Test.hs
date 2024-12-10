module InterpLayer.Test where
import qualified CoreLang.CoreSorts as Core
import qualified Hoki.FrontSorts as Hoki
import Data.List(foldl')
import Control.Exception

data InterpException = UnhandledType | Unimplemented
    deriving (Show)
instance Exception InterpException

hokiToCoreLit :: Hoki.Literal -> Core.Literal
hokiToCoreLit (Hoki.LInt x) = Core.LInt (fromInteger x)
hokiToCoreLit (Hoki.LDec x) = Core.LDouble x
hokiToCoreLit (Hoki.LChar x) = Core.LChar x
hokiToCoreLit (Hoki.LStr x) = throw UnhandledType
hokiToCoreLit (Hoki.LBool x) = throw UnhandledType

hokiToCoreArg :: Hoki.Args -> Core.Expr
hokiToCoreArg (Hoki.Avar name) = Core.EVar name
hokiToCoreArg (Hoki.Alit literal) = Core.ELit (hokiToCoreLit literal)

hokiToCoreExpr :: Hoki.Expr -> Core.Expr
hokiToCoreExpr (Hoki.Evar name) = Core.EVar name
hokiToCoreExpr (Hoki.Elit literal) = Core.ELit (hokiToCoreLit literal)
hokiToCoreExpr (Hoki.Eapp func args) = let
    args' = map hokiToCoreArg args
    func' = Core.EVar func
    in case args' of
        [] -> undefined
        (x:xs) -> foldl' (Core.EApp) (Core.EApp func' x) xs
hokiToCoreExpr (Hoki.Eabb name args exprs types) = undefined
hokiToCoreExpr (Hoki.Econs x) = undefined
