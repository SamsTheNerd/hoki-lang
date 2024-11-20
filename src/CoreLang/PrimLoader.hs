module CoreLang.PrimLoader where

import CoreLang.PrimIntegers
import CoreLang.CoreSorts
import Data.Map (insert, fromList, empty)
import CoreLang.Runad
import CoreLang.CoreEval (eval)
import CoreLang.Typad (TConsLookup, VarTyEnv, TError, runTypad, TypadST (TypadST))
import CoreLang.CoreTyping (inferTypeTL)
import Data.IORef (newIORef)

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

loadProgEnvT :: Program -> (VarTyEnv, TConsLookup, DCLookup)
loadProgEnvT = foldr loadStatementTEnv (empty, empty, empty)

-- loads a single statement into the env
loadStatementREnv :: Statement -> (VarEnv, DCLookup) -> (VarEnv, DCLookup)
loadStatementREnv stmt@(SLetRec vid bexp annot) (venv, dcl) = (insert vid bexp venv, dcl)
loadStatementREnv stmt@(STypeDef (TypeCons _ _ dcs)) (venv, dcl)
    = foldr (\dc@(DataCons did _) 
        (venv', dcl') -> (insert did (mkDCLambda dc) venv', insert did dc dcl'))
        (venv, dcl) dcs

loadStatementTEnv :: Statement -> (VarTyEnv, TConsLookup, DCLookup) -> (VarTyEnv, TConsLookup, DCLookup)
loadStatementTEnv stmt@(SLetRec vid bexp annot) (venv, tcl, dcl) = (venv, tcl, dcl) -- come back to this
loadStatementTEnv stmt@(STypeDef tcon@(TypeCons name tvs dcs)) (venv, tcl, dcl) = foldr (\dc@(DataCons did _) 
        (venv', tcl', dcl') -> (insert did (typeDCLambda tcon dc) venv', tcl',  insert did dc dcl'))
        (venv, insert name tcon tcl , dcl) dcs

-- takes a data constructor and returns the type of its lambda
typeDCLambda :: TypeCons -> DataCons -> Type
typeDCLambda tcon@(TypeCons name tvs dcs) dc@(DataCons did ts) = 
    if null tvs then bodyT else TQuant (map (\tv -> (tv,CExact $ TVar tv)) tvs) bodyT
    where bodyT = foldr TArrow (TCon name (map TVar tvs)) ts -- TODO: this should probably get quantified? do that later :p

-- makes a lambda to be used for constructing datacons
mkDCLambda :: DataCons -> Expr
mkDCLambda dc@(DataCons did ts) = foldr ELambda (ECons did (map EVar mts)) mts
    where mts = zipWith (\_ i -> "dc" ++ show i) ts (iterate (+1) 0) -- labeled identifiers

evalProgram :: Program -> Expr -> IO (Either EError Expr)
evalProgram prog expr = fst <$> runRunAd (eval expr) (uncurry RunadSTIn (loadProgEnv prog) 0) 

inferInProgram :: Program -> Expr -> IO (Either TError Type)
inferInProgram prog expr = do
        fref <- newIORef 0
        runTypad (inferTypeTL expr) (TypadST venv tcl dcl fref) 
    where (venv, tcl, dcl) = loadProgEnvT prog