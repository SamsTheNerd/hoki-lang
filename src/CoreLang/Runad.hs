module CoreLang.Runad where
import Data.Map ( empty, insert, lookup, Map, singleton )
import CoreLang.CoreSorts
import Control.Monad

-- a monad to structure execution and whatnot
-- mostly for passing subst maps, alpha renaming, and 

-- (runad types are defined in CoreSorts to avoid circular deps)

runRunAd :: Runad a -> RunadSTIn -> IO (Either EError a, RunadSTOut)
runRunAd (Runad rad) = rad

-- sensibly drop all unneeded in data
passInOut :: RunadSTIn -> RunadSTOut
passInOut (RunadSTIn venv dcl fr) = RunadSTOut fr

testAd rad = runRunAd rad (RunadSTIn empty empty 0)

instance Functor Runad where 
    fmap f rad = Runad $ \st -> do
        (res, st') <- runRunAd rad st 
        return (f <$> res, st')

instance Monad Runad where
    return = pure

    rad1 >>= f = Runad $ \st@(RunadSTIn env dcl fint) -> do
        (res, st'@(RunadSTOut fint')) <- runRunAd rad1 st 
        case res of
            (Right pl) -> runRunAd (f pl) (RunadSTIn env dcl fint')
            (Left err) -> return (Left err, st')
        

instance Applicative Runad where
    pure x = Runad $ \st -> pure (Right x, passInOut st)

    (<*>) = ap

-- prefer these semi-internal?

getRST :: Runad RunadSTIn
getRST = Runad (\st -> return (Right st, passInOut st))

modRST :: (RunadSTIn -> RunadSTOut) -> Runad ()
modRST f = Runad (\st -> return (Right (), f st))

-- modRST' :: (VarEnv -> VarEnv) -> (Int -> Int) -> Runad ()
-- modRST' envF fintF = Runad $ \(RunadST env fint) 
--     -> return (Left (), RunadST (envF env) (fintF fint))

-- stGetEnv :: RunadST -> VarEnv
-- stGetEnv (RunadST env _ ) = env;

-- stModEnv :: (VarEnv -> VarEnv) -> RunadST -> RunadST
-- stModEnv f (RunadST env fr) = RunadST (f env ) fr

-- stGetFInt :: RunadST -> Int
-- stGetFInt (RunadST _ fint) = fint

-- stModFInt :: (Int -> Int) -> RunadST -> RunadST
-- stModFInt f (RunadST env fr) = RunadST env (f fr)

-- can use these 

runadErr :: EError -> Runad a
runadErr err = Runad $ \st -> return (Left err, passInOut st)

-- alpha renames the given identifier to a new name
gecrFresh :: Ident -> Runad Ident
gecrFresh vid = do
    st@(RunadSTIn _ _ fint) <- getRST
    modRST (\(RunadSTIn env dcl fint') -> RunadSTOut fint')
    return $ vid ++ show fint

lookupVar :: Ident -> Runad (Maybe Expr)
lookupVar v = do
    (RunadSTIn env _ _) <- getRST
    return $ Data.Map.lookup v env

lookupDC :: Ident -> Runad (Maybe (DataCons, TypeCons))
lookupDC v = do
    (RunadSTIn _ dcl _) <- getRST
    return $ Data.Map.lookup v dcl
    
lookupVar' :: Ident -> Runad Expr
lookupVar' v = do
    (RunadSTIn env _ _) <- getRST
    case Data.Map.lookup v env of
        (Just expr) -> return expr
        _ -> runadErr $ "Undefined variable " ++ v

-- currently *only* binds the given variable. does not substitute its value in others
-- extendVarEnv :: Ident -> Expr -> Runad ()
-- bindVar v expr = do
--     mod

-- runs the given monad in the extended environment
extendVarEnv :: Ident -> Expr -> Runad a -> Runad a
extendVarEnv v expr = extendVarEnv' (singleton v expr)

extendVarEnv' :: VarEnv -> Runad a -> Runad a
extendVarEnv' venv inad = Runad $ \(RunadSTIn env dcl fint) -> 
    runRunAd inad (RunadSTIn (venv <> env) dcl fint)