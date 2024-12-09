module CoreLang.PrimIntegers where

import CoreLang.CoreSorts
import CoreLang.Runad
import CoreLang.CoreEval
import Control.Monad ((>=>))

primIntOps :: [(String, Expr)]
primIntOps = [
    ("intAdd", EPrimOp intAdd),
    ("intMul", EPrimOp intMul),
    ("intNeg", EPrimOp intNeg)
    ]

getAsInt :: Expr -> Runad Int
getAsInt exp = do
    r <- eval exp
    case r of
        (ELit (LInt i)) -> return i
        _ -> runadErr $ "expected int but got " ++ show exp
        -- ^^^ THIS SHOULD NEVER HAPPEN IF TYPE CHECKER IS WORKING --

partialIntAdd :: Int -> PrimOp
partialIntAdd n = PrimOp (fmap (ELit . LInt . (n +)) . getAsInt) (TArrow primIntT primIntT) ("( + " ++ show n ++ " )")

intAdd :: PrimOp
intAdd = PrimOp (fmap (EPrimOp . partialIntAdd) . getAsInt) (TArrow primIntT (TArrow primIntT primIntT)) " (+) "

partialIntMul :: Int -> PrimOp
partialIntMul n = PrimOp (fmap (ELit . LInt . (n *)) . getAsInt) (TArrow primIntT primIntT) ("( * " ++ show n ++ " )")

intMul :: PrimOp
intMul = PrimOp (fmap (EPrimOp . partialIntMul) . getAsInt) (TArrow primIntT (TArrow primIntT primIntT)) "(*)"

intNeg :: PrimOp
intNeg = PrimOp (fmap (ELit . LInt . negate) . getAsInt) (TArrow primIntT primIntT) "negate"