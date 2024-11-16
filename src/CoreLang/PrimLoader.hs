module CoreLang.PrimLoader where

import CoreLang.PrimIntegers
import CoreLang.CoreSorts
import Data.Map (insert, fromList, empty)
import CoreLang.Runad
import CoreLang.CoreEval (eval)

-- gathering primitve groups

primGroups :: [[(String, Expr)]]
primGroups = [
    primIntOps
    ]

allPrimOps :: [(String, Expr)]
allPrimOps = concat primGroups

-- load Runad Env from Program (assume any errors were thrown during type checking load)

loadProgEnv :: Program -> (VarEnv, DCLookup)
loadProgEnv = foldr loadStatementREnv (fromList allPrimOps, empty)

-- loads a single statement into the env
loadStatementREnv :: Statement -> (VarEnv, DCLookup) -> (VarEnv, DCLookup)
loadStatementREnv stmt@(SLetRec vid bexp annot) (venv, dcl) = (insert vid bexp venv, dcl)
loadStatementREnv stmt@(STypeDef (TypeCons _ _ dcs)) (venv, dcl)
    = foldr (\dc@(DataCons did _) 
        (venv', dcl') -> (insert did (mkDCLambda dc) venv', insert did dc dcl'))
        (venv, dcl) dcs

-- makes a lambda to be used for constructing datacons
mkDCLambda :: DataCons -> Expr
mkDCLambda dc@(DataCons did ts) = foldr ELambda (ECons did (map EVar mts)) mts
    where mts = zipWith (\_ i -> "dc" ++ show i) ts (iterate (+1) 0) -- labeled identifiers


evalProgram :: Program -> Expr -> IO (Either EError Expr)
evalProgram prog expr = fst <$> runRunAd (eval expr) (uncurry RunadSTIn (loadProgEnv prog) 0) 