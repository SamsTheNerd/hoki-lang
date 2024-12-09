module CoreLang.Typad where
import CoreLang.CoreSorts
import Data.Map hiding ((\\), map)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import Control.Monad (ap, void, zipWithM, when)
import Data.List (nub, (\\))

-- a monad for the typing environment. 

-- the design here is __heavily__ influenced by the TcMonad of Practical type inference for arbitrary-rank types (https://www.cambridge.org/core/journals/journal-of-functional-programming/article/practical-type-inference-for-arbitraryrank-types/5339FB9DAB968768874D4C20FA6F8CB6)
-- Since that paper is presented in a tutorial style, it has code available. For our own learning we won't be using that code directly but will likely reference it often. and as mentioned before, we're using the TcMonad design as a starting point.
-- We don't need the higher rank typing, but we will be using a bidirectional HM-ish approach similar to the paper. This allows for subtyping relations to be more easily added to the type system in the future.

-- map of bound *expression* variables to their types. 
type VarTyEnv = Map Ident Type

-- map of data constructor names to their actual data constructors and the type constructor that they're for.
type TConsLookup = Map Ident TypeCons
-- type TVarLookup = Map TVar Type

type TError = String

data TypadST = TypadST
    VarTyEnv -- needs to be scoped
    TConsLookup
    DCLookup
    (IORef Int) -- so that we can generate fresh meta vars - these can be IO refs because we just need uniqueness so global state is fine

newtype Typad a = Typad (TypadST -> IO (Either TError a))

runTypad :: Typad a -> TypadST -> IO (Either TError a)
runTypad (Typad f) = f

-- throw an error
typadErr :: TError -> Typad a
typadErr err = Typad (\st -> return $ Left err)

instance Functor Typad where
    fmap f tad = Typad $ \st -> do
        res <- runTypad tad st
        return $ f <$> res

instance Monad Typad where
    return = pure

    tad1 >>= f = Typad $ \st -> do
        res <- runTypad tad1 st
        case res of
            (Right pl) -> runTypad (f pl) st
            (Left err) -> return $ Left err

instance Applicative Typad where
    pure x = Typad $ \st -> pure $ Right x

    (<*>) = ap



-- gets a fresh name for metavars and such
getFreshName :: Typad Int
getFreshName = Typad $ \(TypadST _ _ _ fref) -> do
    v <- readIORef fref
    modifyIORef' fref (+1)
    return $ Right v

-- lifts IO monads into this monad
lift :: IO a -> Typad a
lift iom = Typad $ \st -> Right <$> iom

newMetaTVar :: Typad MetaTVar
newMetaTVar = do
    ref <- lift $ newIORef CAny
    name <- getFreshName
    return $ MetaTVar name ref

newMetaTVarWith :: TConstraint -> Typad MetaTVar
newMetaTVarWith cons = do
    ref <- lift $ newIORef cons
    name <- getFreshName
    return $ MetaTVar name ref

readMetaTVar :: MetaTVar -> Typad TConstraint
readMetaTVar (MetaTVar _ ref) = lift $ readIORef ref
readMetaTVar (MetaStrVar _ ref) = lift $ readIORef ref

writeMetaTVar :: MetaTVar -> TConstraint -> Typad ()
writeMetaTVar mv@(MetaTVar _ ref) tcon = do 
    zcon <- zonkConstraint tcon
    lift $ writeIORef ref zcon; 
    -- lift $ putStrLn ("writing " ++ show mv ++ " as: " ++ show tcon ++ " \t(z=>)\t " ++ show zcon)
writeMetaTVar mv@(MetaStrVar _ ref) tcon = do
    zcon <- zonkConstraint tcon
    lift $ writeIORef ref zcon
    -- lift $ putStrLn ("writing " ++ show mv ++ " as: " ++ show tcon ++ " \t(z=>)\t " ++ show zcon)

-- newTypeVar :: Typad Type
-- -- newTypeVar = TVar . ("tv" ++) . show <$> getFreshName
-- newTypeVar = do
--     i <- getFreshName
--     TMetaVar <$> newMetaTVarWith ((CExact . TVar . ("tv" ++) . show) i)



------------------------------------------------------------------------------
--              State/Env Handling
------------------------------------------------------------------------------

lookupVarTy :: Ident -> Typad Type
lookupVarTy vid = Typad $ \(TypadST venv _ _ _) -> return $ case Data.Map.lookup vid venv of
    (Just ty) -> Right ty
    _         -> Left $ "Unknown variable: " ++ vid

lookupVarTy' :: Ident -> Typad (Maybe Type)
lookupVarTy' vid = Typad $ \(TypadST venv _ _ _) -> (return . Right . Data.Map.lookup vid) venv

extendVarEnvT :: Ident -> Type -> Typad a -> Typad a
extendVarEnvT vid ty inad = Typad $ \st@(TypadST venv tcl dcl fref) ->
    runTypad inad (TypadST (insert vid ty venv) tcl dcl fref)

extendVarEnvTs :: Map Ident Type -> Typad a -> Typad a
extendVarEnvTs subst inad = Typad $ \(TypadST venv tcl dcl fref) ->
    runTypad inad (TypadST (subst <> venv) tcl dcl fref)

lookupDC :: Ident -> Typad (DataCons, TypeCons)
lookupDC dcid = Typad $ \(TypadST _ _ dcl _) -> return $ case Data.Map.lookup dcid dcl of
    (Just ty) -> Right ty
    _         -> Left $ "Unknown type constructor: " ++ dcid

lookupDC' :: Ident -> Typad (Maybe (DataCons, TypeCons))
lookupDC' dcid = Typad $ \(TypadST _ _ dcl _) -> return . Right $ Data.Map.lookup dcid dcl

lookupTCon :: Ident -> Typad TypeCons
lookupTCon tid = Typad $ \(TypadST _ tcl _ _) -> return $ case Data.Map.lookup tid tcl of
    (Just ty) -> Right ty
    _         -> Left $ "Unknown type constructor: " ++ tid

-- knocks out exactly typed constraints (and should prob do something with other constraints but not my issue rn because they don't exist !!)
zonkType :: Type -> Typad Type
zonkType = zonkType' []

-- we keep track of what we've seen to avoid circular typing
zonkType' :: [MetaTVar] -> Type -> Typad Type
zonkType' _ ty@(TVar _) = return ty
zonkType' _ ty@(TNamed _) = return ty
zonkType' seen ty@(TArrow frT toT) = do
    zfrT <- zonkType' seen frT
    ztoT <- zonkType' seen toT
    -- lift . putStrLn . show $ TArrow zfrT ztoT
    return $ TArrow zfrT ztoT
zonkType' seen ty@(TQuant tvs body) = do 
    mrrp <- TQuant tvs <$> zonkType' seen body
    -- lift . putStrLn . show $ mrrp
    return mrrp
zonkType' seen ty@(TMetaVar mv) = if mv `elem` seen then  typadErr ("circular typing: " ++ show mv ++ " already seen") else do
    cons <- readMetaTVar mv
    case cons of
        CAny -> return ty
        (CExact ty') -> do 
            -- lift . putStrLn . show $ ty'
            zonkType' (mv:seen) ty'
zonkType' seen ty@(TCon cn tys) = do
    mrrp <- TCon cn <$> mapM (zonkType' seen) tys
    -- lift . putStrLn . show $ mrrp
    return mrrp

zonkConstraint :: TConstraint -> Typad TConstraint
zonkConstraint CAny = return CAny
zonkConstraint (CExact ty) = CExact <$> zonkType ty

occursIn :: MetaTVar -> Type -> Bool
occursIn mv (TMetaVar mv') = mv == mv'
occursIn mv (TArrow frT toT) = occursIn mv frT || occursIn mv toT
occursIn mv (TQuant tvs body) = occursIn mv body
occursIn mv (TCon cn tys) = any (occursIn mv) tys
occursIn _ (TVar _) = False
occursIn _ (TNamed _) = False

-- gets all free type vars in the given type
getFreeTVars :: Type -> Typad [TVar]
getFreeTVars ty@(TVar vid) = return [vid]
getFreeTVars ty@(TNamed _) = return []
getFreeTVars ty@(TArrow frT toT) = do
    frTfvs <- getFreeTVars frT
    toTfvs <- getFreeTVars toT
    (return . nub . (++ frTfvs)) toTfvs
getFreeTVars ty@(TMetaVar mv) = do
    res <- readMetaTVar mv
    case res of
        CAny -> return []
        (CExact ty') -> getFreeTVars ty'
getFreeTVars ty@(TQuant vcs body) = do
    bfvs <- getFreeTVars body
    return $ bfvs \\ map fst vcs -- maybe need to filter constraints here?
getFreeTVars ty@(TCon tn ts) = concat <$> mapM getFreeTVars ts

-- gets all meta vars in the given type
getMetaVars :: Type -> Typad [MetaTVar]
getMetaVars ty@(TVar vid) = return []
getMetaVars ty@(TNamed _) = return []
getMetaVars ty@(TArrow frT toT) = do
    frTfvs <- getMetaVars frT
    toTfvs <- getMetaVars toT
    (return . nub . (++ toTfvs)) frTfvs
getMetaVars ty@(TMetaVar mv) = do
    res <- readMetaTVar mv
    case res of
        CAny -> return [mv]
        (CExact ty') -> nub . (++ [mv]) <$> getMetaVars ty'
getMetaVars ty@(TQuant vcs body) = getMetaVars body
getMetaVars ty@(TCon tn ts) = concat <$> mapM getMetaVars ts

-- instantiates a (possibly) polymorphic type
instType :: Type -> Typad Type
instType (TQuant tvs bodyT) = instType bodyT
instType ty = do
    -- (lift . putStrLn) $ "instantiating type " ++ show ty
    fvs <- getFreeTVars ty
    tmap <- fromList <$> mapM (\fv -> (fv,) . TMetaVar <$> newMetaTVar) fvs
    return $ substType tmap ty

-- takes a type and knocks out its metavars for quantified tvars
quantifyType :: Type -> Typad Type
quantifyType ty = do
    mvs <- getMetaVars ty
    let tvs = zipWith (\i _ -> "t" ++ show i) [1..] mvs
    let mvSubst = zipWith (\mv tv -> (mv, TVar tv)) mvs tvs
    let tvars = map ((, CAny)) tvs
    let bodyTy = substMVs (fromList mvSubst) ty
    return $ TQuant tvars bodyTy

------------------------------------------------------------------------------
--              Unification
------------------------------------------------------------------------------

-- we're not quite getting into this yet. it'll (maybe one day) be used for constraint gathering/solving for typeclass sorta things, but the only constraints we have currently are any and exact, so we can just pass their insides down to unifyType
unifyConstraints :: TConstraint -> TConstraint -> Typad TConstraint
-- an any constraint can just defer to the other constraint
unifyConstraints CAny tyc = return tyc
unifyConstraints tyc CAny = return tyc
-- do type unification on their contents, then wrap the result back up in a constraint
unifyConstraints (CExact ty1) (CExact ty2) = CExact <$> unifyType ty1 ty2

-- fine to use unifyType externally if you have pure types (including meta vars) but when you have constraints, it's best to use unifyConstraints.


unifyType :: Type -> Type -> Typad Type
-- two metavars, we merge their constraints and bind it to a new metavar, return that
unifyType ty1@(TMetaVar mv1) ty2@(TMetaVar mv2) = if mv1 == mv2 then return ty1 else do
    -- (lift . putStrLn) $ "unifying 2 metavars " ++ show ty1 ++ " & " ++ show ty2
    mcon1 <- readMetaTVar mv1
    mcon2 <- readMetaTVar mv2
    case (mcon1, mcon2) of
        -- both are unbound so just bind the second type to the first
        (CAny, CAny) -> do writeMetaTVar mv1 (CExact ty2); return ty1
        -- one of them is bound, so just defer to constraint solver?
        _ -> do
            unicon <- unifyConstraints mcon1 mcon2 -- if they can't be unified then we get an error that bubbles up through Typad
            -- otherwise write the new unified constraint back to both metavars
            -- lift $ putStrLn ("unifying mvs: " ++ show mv1 ++ " & " ++ show mv2 ++ ": " ++ show unicon)
            -- case unicon of
            --     (CExact tmv@(TMetaVar _)) -> return tmv
            --     _ -> do
            writeMetaTVar mv1 unicon
            writeMetaTVar mv2 unicon
            return ty1

-- single meta var with any other type, we stick the type in an Exact constraint and unify with the meta var's existing constraint
unifyType ty1@(TMetaVar mv1) ty2 = do
    -- (lift . putStrLn) $ "unifying 1 metavar with type: " ++ show ty1 ++ " & " ++ show ty2
    mcon1 <- readMetaTVar mv1
    -- check for circular issues
    zty2 <- zonkType ty2
    case mcon1 of
        CAny -> do
            mvOccurs <- elem mv1 <$> getMetaVars ty2
            Control.Monad.when mvOccurs $ typadErr $ "cannot unify " ++ show (TMetaVar mv1) ++ " with " ++ show zty2
        _ -> return ()
    unicon <- unifyConstraints mcon1 (CExact ty2)
    writeMetaTVar mv1 unicon
    -- (lift . putStrLn) $ "returning: " ++ show ty1
    return ty1

unifyType ty1 ty2@(TMetaVar mv2) = unifyType ty2 ty1 -- just reuse symmetric case

unifyType ty1@(TArrow frT1 toT1) ty2 = do
    -- plzSlowDown frT1
    -- (lift . putStrLn) $ "unifying functions " ++ show ty1 ++ " & " ++ show ty2
    (frT2, toT2) <- splitFun ty2
    frTU <- unifyType frT1 frT2
    toTU <- unifyType toT1 toT2
    return $ TArrow frTU toTU

unifyType ty1 ty2@(TArrow frT2 toT2) = unifyType ty2 ty1 -- reuse symmetric 

-- is there any case where we need to 'split' the other type like we do with functions?
-- I'm not sure that I see why we would?
unifyType ty1@(TCon tn1 ts1) ty2@(TCon tn2 ts2) = if tn1 /= tn2 || length ts1 /= length ts2 then typadErr $ "Could not unify types " ++ show ty1 ++ " with " ++ show ty2 else
    TCon tn1 <$> zipWithM unifyType ts1 ts2

unifyType ty1@(TNamed n1) ty2@(TNamed n2) = if n1 == n2 then return ty1 else typadErr $ " Could not unify types " ++ show ty1 ++ " with " ++ show ty2

unifyType ty1 ty2 = typadErr $ "Unhandled unification of types " ++ show ty1 ++ " and " ++ show ty2

-- splits a function into its separate types, binding them to meta vars if needed
splitFun :: Type -> Typad (Type, Type)
splitFun (TArrow frT toT) = return (frT, toT)
splitFun ty = do
    -- (lift . putStrLn) $ "splitting type " ++ show ty
    frMV <- newMetaTVar
    toMV <- newMetaTVar
    unifyType ty (TArrow (TMetaVar frMV) (TMetaVar toMV))
    return (TMetaVar frMV, TMetaVar toMV)

plzSlowDown :: Type -> Typad ()
plzSlowDown ty@(TMetaVar (MetaTVar n _)) = if n > 25 then typadErr "WOAH TOO FAST" else return ()
plzSlowDown _ = return ()