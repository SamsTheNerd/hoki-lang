module CoreLang.CoreLoader where

import CoreLang.PrimIntegers
import CoreLang.CoreSorts
import Data.Map (insert, fromList, empty, lookup, toList, singleton, union, keys)
import CoreLang.Runad
import CoreLang.CoreEval (eval)
import CoreLang.Typad (TConsLookup, VarTyEnv, TError, runTypad, TypadST (TypadST))
import CoreLang.CoreTyping (inferTypeTL, inferTypeTLRec)
import Data.IORef (newIORef)
import Data.Graph (graphFromEdges, scc)
import Control.Monad (foldM)
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import CoreLang.PrimDoubles (primDoubleOps)
import CoreLang.PrimMeta (primMetaOps)

-- gathering primitve groups

-- the loaded program
data LProg = LProg VarTyEnv TConsLookup DCLookup VarEnv

emptyLProg_ :: LProg
emptyLProg_ = LProg empty empty empty empty

-- TODO: this is disgusting
coreProg :: IO LProg
coreProg = do
    errOrProg <- loadProgram (loadPrimOps [])
    case errOrProg of
        (Left err) -> putStrLn ("MAJOR ERROR: PRIMITIVES ARE BROKEN: " ++ err) >> return undefined
        (Right lprog) -> return lprog

-- TODO: maybe have a better structure for primops if this gets cumbersome
primGroups :: [[(String, Expr)]]
primGroups = [
    primIntOps,
    primDoubleOps,
    primMetaOps
    ]

allPrimOps :: [(String, Expr)]
allPrimOps = concat primGroups

-- TODO: figure out if we want to annotate these primops? idk
loadPrimOps :: Program -> Program
loadPrimOps prog = prog ++ map (\(name, expr) -> SLetRec name expr Nothing) allPrimOps

-- var env here is to carry expressions out so that we can type check them
loadStatement :: Statement -> LProg -> LProg
loadStatement (SLetRec vid bexp mAnnot) (LProg vtenv tcl dcl venv) = LProg
    (union vtenv $ maybe mempty (singleton vid) mAnnot)
    tcl dcl
    $ insert vid bexp venv
loadStatement (STypeDef tcon@(TypeCons name _ dcs)) (LProg vtenv tcl dcl venv) = foldr (
    \dc@(DataCons did _) (LProg vtenv' tcl' dcl' venv') ->
        LProg (insert did (typeDCLambda tcon dc) vtenv') tcl' (insert did (dc, tcon) dcl') (insert did (mkDCLambda dc) venv')
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
loadProgram prog = staticCheck $ foldr loadStatement emptyLProg_ (loadPrimOps prog)

----------------------------------------
--     Initial Type Checking Pass     --
----------------------------------------

-- takes a loaded program and checks that each statement matches its annotation (or try to infer a type for it if it has no annotation)
staticCheck :: LProg -> IO (Either TError LProg)
staticCheck lprog@(LProg _ _ _ venv) = do
    lprog' <- fillTL lprog
    foldM (\case
            (Right val) -> (checkDepSCC val)
            err -> const $ return err) 
        (Right lprog') deps
    where deps = orderDeps venv

-- fill out top level program statements with metavars
fillTL :: LProg -> IO LProg
fillTL (LProg vtenv tcl dcl venv) = do
    let unTyd = keys venv \\ keys vtenv
    mvs <- fromList <$> mapM (
        \vid -> (vid,) . TMetaVar . MetaStrVar vid 
        <$> newIORef CAny ) unTyd
    return (LProg (mvs <> vtenv) tcl dcl venv)



-- checking that a single scc is fine
checkDepSCC :: LProg -> [Ident] -> IO (Either TError LProg)
checkDepSCC (LProg vtenv tcl dcl venv) [dp] = do
    -- guaranteed to have them
    let expr = fromMaybe undefined $ Data.Map.lookup dp venv
    let ty = fromMaybe undefined $ Data.Map.lookup dp vtenv
    -- get type env with inferred type in there
    vtenvE <- case ty of
            mty@(TMetaVar _) -> do
                -- putStrLn $ "\nstarting type inference for: " ++ show dp
                fref <- newIORef 0
                (\case
                    (Left err) -> Left err
                    (Right infTy) -> Right $ Data.Map.insert dp infTy vtenv
                    ) <$> runTypad (inferTypeTLRec expr mty) (TypadST vtenv tcl dcl fref) 
            (ty') -> return . Right $ vtenv -- TODO: do checking here
    return $ (\vtenv' -> LProg vtenv' tcl dcl venv ) <$> vtenvE

checkDepSCC (LProg vtenv tcl dcl venv) dps = return . Left $ "circular dependencies not yet handled"


-- returns sccs ordered in the direction of requirement
orderDeps :: VarEnv -> [[Ident]]
orderDeps venv = fmap (foldMap mrrp) sccs
    where (graph, vton, _) = graphFromEdges $ foldMap (\(v, ve) -> [(v, v, getFreeVarsE ve)]) (Data.Map.toList venv)
          sccs = scc graph
          mrrp = (\(v, _, _) -> [v]) . vton