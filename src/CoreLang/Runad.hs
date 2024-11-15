module CoreLang.Runad where
import Data.Map
import CoreLang.CoreSorts
import Control.Monad

-- a monad to structure execution and whatnot
-- mostly for passing subst maps, alpha renaming, and 

-- a substitution map of variables to expressions
type VarEnv = Map Ident Expr

type EError = String


-- for info that needs to be pushed down in execution
data RunadSTIn = RunadSTIn VarEnv Int 
    deriving (Show, Eq)

-- info that needs to come back up in execution
data RunadSTOut = RunadSTOut Int
    deriving (Show, Eq)

newtype Runad a = Runad (RunadSTIn -> IO (Either a EError, RunadSTOut))

runRunAd :: Runad a -> RunadSTIn -> IO (Either a EError, RunadSTOut)
runRunAd (Runad rad) = rad

testAd rad = runRunAd rad (RunadSTIn empty 0)

instance Functor Runad where 
    fmap f rad = Runad (\st -> do
        (res, st) <- runRunAd rad st 
        return (case res of
            (Left pl) -> Left (f pl)
            (Right err) -> Right err
            , st)
        )

instance Monad Runad where
    return = pure

    rad1 >>= f = Runad (\st@(RunadSTIn env fint) -> do
        (res, st'@(RunadSTOut fint')) <- runRunAd rad1 st 
        case res of
            (Left pl) -> runRunAd (f pl) (RunadSTIn env fint')
            (Right err) -> return (Right err, st')
        )

instance Applicative Runad where
    pure x = Runad $ \(RunadSTIn env fint) -> pure (Left x, RunadSTOut fint)

    (<*>) = ap

-- prefer these semi-internal?

getRST :: Runad RunadSTIn
getRST = Runad (\st@(RunadSTIn env fint) -> return (Left st, RunadSTOut fint))

modRST :: (RunadSTIn -> RunadSTOut) -> Runad ()
modRST f = Runad (\st -> return (Left (), f st))

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
runadErr err = Runad $ \st@(RunadSTIn env fint) -> return (Right err, RunadSTOut fint)

-- alpha renames the given identifier to a new name
gecrFresh :: Ident -> Runad Ident
gecrFresh vid = do
    (RunadSTIn _ fint) <- getRST
    modRST (\(RunadSTIn env fint) -> RunadSTOut fint)
    return $ vid ++ show fint

lookupVar :: Ident -> Runad (Maybe Expr)
lookupVar v = do
    (RunadSTIn env fint) <- getRST
    return $ Data.Map.lookup v env

lookupVar' :: Ident -> Runad Expr
lookupVar' v = do
    (RunadSTIn env fint) <- getRST
    case Data.Map.lookup v env of
        (Just expr) -> return expr
        _ -> runadErr $ "Undefined variable " ++ v

-- currently *only* binds the given variable. does not substitute its value in others
-- extendVarEnv :: Ident -> Expr -> Runad ()
-- bindVar v expr = do
--     mod

-- runs the given monad in the extended environment
extendVarEnv :: Ident -> Expr -> Runad a -> Runad a
extendVarEnv v expr inad = Runad $ \(RunadSTIn env fint) -> 
    runRunAd inad (RunadSTIn (insert v expr env) fint)