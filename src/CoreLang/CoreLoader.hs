{-# LANGUAGE LambdaCase #-}
module CoreLang.PrimLoader where

import CoreLang.PrimIntegers
import CoreLang.CoreSorts
import Data.Map (insert, fromList, empty)
import CoreLang.Runad
import CoreLang.CoreEval (eval)
import Data.List ((\\))
import CoreLang.CoreParser
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except

-- gathering primitve groups

primGroups :: [ProgLink]
primGroups = [
    Direct "primOps" primIntOps
    ]

coreLibs = [
    "lists"
    ]

-- load Runad Env from Program (assume any errors were thrown during type checking load)
defaultImports :: [ProgLink]
defaultImports = primGroups ++ map CoreLib coreLibs

-- gatherImports takes a set of already found imports, imports to newly load, and returns a set of all fully loaded imports as well as the loaded programs
gatherImports :: [ProgLink] -> [ProgLink] -> IO (Either String [(ProgLink, Program)])
gatherImports found finding = undefined
    where finding' = finding \\ found -- make sure we're not loading any we've already found
-- gatherImports prog = 
--     let imps = Data.Set.fromList $ concatMap (\case (SImport pl) -> [pl]; _ -> []) prog

-- shallow !
getModuleDeps :: Program -> [ProgLink]
getModuleDeps = concatMap (\case (SImport pl) -> [pl]; _ -> [])

loadProgImport :: ProgLink -> ExceptT IO Program
loadProgImport (Direct _ prog) -> 

loadProgEnv :: Program -> (VarEnv, DCLookup)
loadProgEnv = foldr loadStatementREnv (empty, empty)

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