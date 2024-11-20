module CoreLang.CoreLoader where

import CoreLang.PrimIntegers
import CoreLang.CoreSorts
import Data.Map (insert, fromList, empty, lookup, toList, singleton, union)
import CoreLang.Runad
import CoreLang.CoreEval (eval)
import CoreLang.Typad (TConsLookup, VarTyEnv, TError, runTypad, TypadST (TypadST))
import CoreLang.CoreTyping (inferTypeTL)
import Data.IORef (newIORef)
import Data.Graph (graphFromEdges, scc)
import Control.Monad (foldM)

-- gathering primitve groups

-- the loaded program
data LProg = LProg VarTyEnv TConsLookup DCLookup VarEnv -- TODO: should this like,, be a data type?

emptyLProg :: LProg
emptyLProg = LProg empty empty empty empty


primGroups :: [[(String, Expr)]]
primGroups = [
    primIntOps
    ]

allPrimOps :: [(String, Expr)]
allPrimOps = concat primGroups

-- var env here is to carry expressions out so that we can type check them
loadStatement :: Statement -> LProg -> LProg
loadStatement (SLetRec vid bexp mAnnot) (LProg vtenv tcl dcl venv) = LProg
    (union vtenv $ maybe mempty (singleton vid) mAnnot)
    tcl dcl
    $ insert vid bexp venv
loadStatement (STypeDef tcon@(TypeCons name _ dcs)) (LProg vtenv tcl dcl venv) = foldr (
    \dc@(DataCons did _) (LProg vtenv' tcl' dcl' venv') ->
        LProg (insert did (typeDCLambda tcon dc) vtenv') tcl' (insert did dc dcl') (insert did (mkDCLambda dc)venv')
    )
    (LProg vtenv (insert name tcon tcl) dcl venv) dcs

-- takes a data constructor and returns the type of its lambda
typeDCLambda :: TypeCons -> DataCons -> Type
typeDCLambda (TypeCons name tvs _) (DataCons _ ts) =
    if null tvs then bodyT else TQuant (map (\tv -> (tv,CExact $ TVar tv)) tvs) bodyT
    where bodyT = foldr TArrow (TCon name (map TVar tvs)) ts -- TODO: this should probably get quantified? do that later :p

-- makes a lambda to be used for constructing datacons
mkDCLambda :: DataCons -> Expr
mkDCLambda (DataCons did ts) = foldr ELambda (ECons did (map EVar mts)) mts
    where mts = zipWith (\_ i -> "dc" ++ show i) ts (iterate (+1) 0) -- labeled identifiers

evalProgram :: LProg -> Expr -> IO (Either EError Expr)
evalProgram (LProg _ _ dcl venv) expr = fst <$> runRunAd (eval expr) (RunadSTIn venv dcl 0)

-- arguably this should not be loaded every time
inferInProgram :: LProg -> Expr -> IO (Either TError Type)
inferInProgram prog expr = do
        -- type check expression in program
        fref <- newIORef 0
        runTypad (inferTypeTL expr) (TypadST vtenv tcl dcl fref)
    where (LProg vtenv tcl dcl _) = prog

-- loads and types checks the given program or throws an error
loadProgram :: Program -> IO (Either TError LProg)
loadProgram prog = staticCheck $ foldr loadStatement emptyLProg prog

----------------------------------------
--     Initial Type Checking Pass     --
----------------------------------------

-- takes a loaded program and checks that each statement matches its annotation (or try to infer a type for it if it has no annotation)
staticCheck :: LProg -> IO (Either TError LProg)
staticCheck lprog@(LProg _ _ _ venv) = foldM (\case
    (Right val) -> (checkDepSCC val)
    err -> const $ return err) (Right lprog) deps
    where deps = orderDeps venv 

-- checking that a single scc is fine
checkDepSCC :: LProg -> [Ident] -> IO (Either TError LProg)
checkDepSCC lprog@(LProg vtenv tcl dcl venv) [dp] = case (Data.Map.lookup dp vtenv, Data.Map.lookup dp venv) of
    -- unknown variable, throw error
    (_, Nothing) -> return . Left $ "unknown variable " ++ show dp
    -- has an annotation so typecheck
    (Just ty, Just ve) -> return . Right $ lprog -- TODO: actually check stuff here
    -- no annotation so infer one
    (Nothing, Just ve) -> do 
        fref <- newIORef 0
        runTypad (inferTypeTL ve) (TypadST vtenv tcl dcl fref) >>= \case
            (Left err) -> return . Left $ err
            (Right infTy) -> return . Right $ LProg (insert dp infTy vtenv) tcl dcl venv

checkDepSCC (LProg vtenv tcl dcl venv) dps = return . Left $ "circular dependencies not yet handled"


-- returns sccs ordered in the direction of requirement
orderDeps :: VarEnv -> [[Ident]]
orderDeps venv = fmap (foldMap mrrp) sccs
    where (graph, vton, _) = graphFromEdges $ foldMap (\(v, ve) -> [(v, v, getFreeVarsE ve)]) (Data.Map.toList venv)
          sccs = scc graph
          mrrp = (\(v, _, _) -> [v]) . vton