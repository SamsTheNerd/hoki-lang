module CoreLang.PrimMeta where

import CoreLang.CoreSorts
import CoreLang.Runad
import CoreLang.CoreEval
import Control.Monad ((>=>))
import GHC.Float ()

primMetaOps :: [(String, Expr)]
primMetaOps = [
    ("evalStrict", EPrimOp strictEvalOp)
    ]

strictEvalOp :: PrimOp
strictEvalOp = PrimOp evalStrict (TQuant [("a", CAny)] (TArrow (TVar "a") (TVar "a"))) "id"

-- getAsInt :: Expr -> Runad Int
-- getAsInt exp = do
--     r <- eval exp
--     case r of
--         (ELit (LInt i)) -> return i
--         _ -> runadErr $ "expected int but got " ++ show exp
--         -- ^^^ THIS SHOULD NEVER HAPPEN IF TYPE CHECKER IS WORKING --

-- partialIntAdd :: Int -> PrimOp
-- partialIntAdd n = PrimOp (fmap (ELit . LInt . (n +)) . getAsInt) (TArrow primIntT primIntT) ("( + " ++ show n ++ " )")

-- intAdd :: PrimOp
-- intAdd = PrimOp (fmap (EPrimOp . partialIntAdd) . getAsInt) (TArrow primIntT (TArrow primIntT primIntT)) " (+) "
