module CoreLang.Typad where
import CoreLang.CoreSorts
import Data.Map hiding (map)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef', writeIORef)
import Control.Monad (ap, void)

-- a monad for the typing environment. 

-- the design here is __heavily__ influenced by the TcMonad of Practical type inference for arbitrary-rank types (https://www.cambridge.org/core/journals/journal-of-functional-programming/article/practical-type-inference-for-arbitraryrank-types/5339FB9DAB968768874D4C20FA6F8CB6)
-- Since that paper is presented in a tutorial style, it has code available. For our own learning we won't be using that code directly but will likely reference it often. and as mentioned before, we're using the TcMonad design as a starting point.
-- We don't need the higher rank typing, but we will be using a bidirectional HM-ish approach similar to the paper. This allows for subtyping relations to be more easily added to the type system in the future.

-- map of bound *expression* variables to their types. 
type VarTyEnv = Map Ident Type

-- map of data constructor names to their actual data constructors and the type constructor that they're for.
type TConsLookup = Map Ident (DataCons, TypeCons)

type TError = String

data TypadST = TypadST
    VarTyEnv -- needs to be scoped
    TConsLookup
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
getFreshName = Typad $ \(TypadST _ _ fref) -> do
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

writeMetaTVar :: MetaTVar -> TConstraint -> Typad ()
writeMetaTVar (MetaTVar _ ref) tcon = lift $ writeIORef ref tcon


-- newTypeVar :: Typad Type
-- -- newTypeVar = TVar . ("tv" ++) . show <$> getFreshName
-- newTypeVar = do
--     i <- getFreshName
--     TMetaVar <$> newMetaTVarWith ((CExact . TVar . ("tv" ++) . show) i)



------------------------------------------------------------------------------
--              State/Env Handling
------------------------------------------------------------------------------

lookupVarTy :: Ident -> Typad Type
lookupVarTy vid = Typad $ \(TypadST venv _ _) -> return $ case Data.Map.lookup vid venv of
    (Just ty) -> Right ty
    _         -> Left $ "Unknown variable: " ++ vid

lookupVarTy' :: Ident -> Typad (Maybe Type)
lookupVarTy' vid = Typad $ \(TypadST venv _ _) -> (return . Right . Data.Map.lookup vid) venv

extendVarEnvT :: Ident -> Type -> Typad a -> Typad a
extendVarEnvT vid ty inad = Typad $ \st@(TypadST venv dcl fref) ->
    runTypad inad (TypadST (insert vid ty venv) dcl fref)

-- knocks out exactly typed constraints (and should prob do something with other constraints but not my issue rn because they don't exist !!)
zonkType :: Type -> Typad Type
zonkType ty@(TVar _) = return ty
zonkType ty@(TNamed _) = return ty
zonkType ty@(TArrow frT toT) = do
    zfrT <- zonkType frT
    ztoT <- zonkType toT
    return $ TArrow zfrT ztoT
zonkType ty@(TQuant tvs body) = TQuant tvs <$> zonkType body
zonkType ty@(TMetaVar mv) = do
    cons <- readMetaTVar mv
    case cons of
        CAny -> return ty
        (CExact ty') -> zonkType ty'
zonkType ty@(TCon cn tys) = TCon cn <$> mapM zonkType tys

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
    mcon1 <- readMetaTVar mv1
    mcon2 <- readMetaTVar mv2
    case (mcon1, mcon2) of
        -- both are unbound so just bind the second type to the first
        (CAny, CAny) -> do writeMetaTVar mv1 (CExact ty2); return ty1
        -- one of them is bound, so just defer to constraint solver?
        _ -> do 
            unicon <- unifyConstraints mcon1 mcon2 -- if they can't be unified then we get an error that bubbles up through Typad
            -- otherwise write the new unified constraint back to both metavars
            writeMetaTVar mv1 unicon
            writeMetaTVar mv2 unicon
            return ty1

-- single meta var with any other type, we stick the type in an Exact constraint and unify with the meta var's existing constraint
unifyType ty1@(TMetaVar mv1) ty2 = do
    mcon1 <- readMetaTVar mv1
    unicon <- unifyConstraints mcon1 (CExact ty2)
    writeMetaTVar mv1 unicon
    return ty1

unifyType ty1 ty2@(TMetaVar mv2) = unifyType ty2 ty1 -- just reuse symmetric case

unifyType ty1@(TArrow frT1 toT1) ty2 = do
    (frT2, toT2) <- splitFun ty2
    unifyType frT1 frT2
    unifyType toT1 toT2

unifyType ty1 ty2@(TArrow frT2 toT2) = unifyType ty2 ty1 -- reuse symmetric 


-- splits a function into its separate types, binding them to meta vars if needed
splitFun :: Type -> Typad (Type, Type)
splitFun (TArrow frT toT) = return (frT, toT)
splitFun ty = do
    frMV <- newMetaTVar
    toMV <- newMetaTVar
    unifyType ty (TArrow (TMetaVar frMV) (TMetaVar toMV))
    return (TMetaVar frMV, TMetaVar toMV)
