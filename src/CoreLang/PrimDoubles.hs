module CoreLang.PrimDoubles where

import CoreLang.CoreSorts
import CoreLang.Runad
import CoreLang.CoreEval
import Control.Monad ((>=>))

primDoubleOps :: [(String, Expr)]
primDoubleOps = [
    ("dbAdd", EPrimOp dbAdd),
    ("dbMul", EPrimOp dbMul),
    ("dbDiv", EPrimOp dbDiv),
    ("dbNeg", EPrimOp dbNeg)
    ]

getAsDouble :: Expr -> Runad Double
getAsDouble exp = do
    r <- eval exp
    case r of
        (ELit (LDouble i)) -> return i
        _ -> runadErr $ "expected double but got " ++ show exp
        -- ^^^ THIS SHOULD NEVER HAPPEN IF TYPE CHECKER IS WORKING --

partialDoubleAdd :: Double -> PrimOp
partialDoubleAdd n = PrimOp (fmap (ELit . LDouble . (n +)) . getAsDouble) (TArrow primDoubleT primDoubleT) ("( + " ++ show n ++ " )")

dbAdd :: PrimOp
dbAdd = PrimOp (fmap (EPrimOp . partialDoubleAdd) . getAsDouble) (TArrow primDoubleT (TArrow primDoubleT primDoubleT)) " (+) "

partialDoubleMul :: Double -> PrimOp
partialDoubleMul n = PrimOp (fmap (ELit . LDouble . (n *)) . getAsDouble) (TArrow primDoubleT primDoubleT) ("( * " ++ show n ++ " )")

dbMul :: PrimOp
dbMul = PrimOp (fmap (EPrimOp . partialDoubleMul) . getAsDouble) (TArrow primDoubleT (TArrow primDoubleT primDoubleT)) "(*)"

dbNeg :: PrimOp
dbNeg = PrimOp (fmap (ELit . LDouble . negate) . getAsDouble) (TArrow primDoubleT primDoubleT) "negate"


partialDoubleDiv :: Double -> PrimOp
partialDoubleDiv n = PrimOp (fmap (\m -> if m == 0 then ECons "Nothing" [] else ELit . LDouble $ (n/m)) . getAsDouble) 
    (TArrow primDoubleT primDoubleT) ("(" ++ show n ++ " /)")

-- is safe (and returns Maybes which,, aren't really supported at core level? idk don't worry about it)
dbDiv :: PrimOp
dbDiv = PrimOp (fmap (EPrimOp . partialDoubleDiv) . getAsDouble) (TArrow primDoubleT (TArrow primDoubleT (TCon "Maybe" [primDoubleT]))) "(/)"