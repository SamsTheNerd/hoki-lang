module CoreLang.CoreRepl where
import Control.Monad.State.Strict (StateT (..), lift, put, get, MonadIO (liftIO), gets, withStateT)
import CoreLang.CoreSorts
import CoreLang.CoreParser (readProgramFile, parseExpr)
import CoreLang.CoreLoader (evalProgram, inferInProgram, LProg (LProg), loadProgram, coreProg)
import Data.Bifunctor (Bifunctor(..))
import Control.Monad (void, liftM2)
import System.Console.Haskeline
import Data.Maybe (fromMaybe)
import CoreLang.CoreTyping (inferType, inferTypeTL)
import CoreLang.Typad (TypadST(TypadST), runTypad)
import Data.Map (empty, keys)
import Data.IORef (newIORef)
import CoreLang.CoreCompiler (haskellifyExpr, haskellifyProg)

-- Core Repl Monad
type CReplad a = InputT (StateT (Maybe FilePath, LProg) IO) a

creplDispatch :: String -> String -> CReplad ()
creplDispatch "l" arg = do
    errOrProg <- liftIO $ readProgramFile arg
    errOrLProg <- case errOrProg of
        (Left err) -> liftIO . return . Left $  ("error: " ++ show err)
        (Right prog) -> liftIO $ loadProgram prog
    case errOrLProg of
        (Left err) -> liftIO . putStrLn $ "error: " ++ show err
        (Right lprog) -> (lift . put $ (Just arg, lprog)) >> liftIO (putStrLn $ "loaded file: " ++ arg)
creplDispatch "r" _ =lift get >>= \case
    (Just fn, _) -> do
        errOrProg <- liftIO $ readProgramFile fn
        errOrLProg <- case errOrProg of
            (Left err) -> liftIO . return . Left $  ("error: " ++ show err)
            (Right prog) -> liftIO $ loadProgram prog
        case errOrLProg of
            (Left err) -> liftIO . putStrLn $ "error: " ++ show err
            (Right lprog) -> (lift . put $ (Just fn, lprog)) >> liftIO (putStrLn $ "loaded file: " ++ fn)
    (Nothing, _) -> outputStrLn "no program loaded"
creplDispatch "t" expr = creplInferType expr
creplDispatch "h" expr = creplAct (\_ -> return . haskellifyExpr) expr
creplDispatch "hl" _ = do
    mayFP <- lift (gets fst)
    mrrp <- liftIO $ maybe (return $ Right []) readProgramFile mayFP
    outputStrLn $ either show haskellifyProg mrrp 
creplDispatch "tAll" _= lift (gets snd) >>= \(LProg _ _ _ venv) -> mapM_ creplInferType (keys venv)
creplDispatch cmd arg = outputStrLn $ "unknown command :"  ++ cmd

creplInferType :: String -> CReplad ()
creplInferType expr = creplAct ((\case
            (Left err) -> ("error: " ++ err)
            (Right resVal) -> show expr ++ " :: " ++ show resVal)
        <$:> inferInProgram) expr

creplEval :: String -> CReplad ()
creplEval expr = creplAct ((\case
            (Left err) -> ("error: " ++ err)
            (Right resVal) -> show resVal)
        <$:> evalProgram) expr

(<$:>) :: Monad m => (c -> d) -> (a -> b -> m c) -> a -> b -> m d
af <$:> f = \x y -> af <$> f x y

creplAct :: (LProg -> Expr -> IO String) -> String -> CReplad ()
creplAct f inp = case parseExpr inp of
    (Left err) -> outputStrLn ("parse error: " ++ show err)
    (Right expr) -> do
        lprog <- lift (gets snd)
        liftIO (f lprog expr) >>= outputStrLn

creplLoop :: CReplad ()
creplLoop = do
    -- liftIO $ putStr "<3: "
    inp <- fromMaybe "" <$> getInputLine "<3: "
    case inp of
        ":" -> outputStrLn "command expected but none received"
        (':':inps) -> creplDispatch cmd (unwords args)
            where (cmd:args) = words inps
        "" -> return ()
        _ ->  creplAct ((\case
            (Left err) -> ("error: " ++ err)
            (Right resVal) -> show resVal)
            <$:> evalProgram) inp
    creplLoop

startCrepl :: IO ()
startCrepl = do
    cprog <- coreProg
    runStateT (runInputT defaultSettings creplLoop) (Nothing, cprog)
    return ()

-- main = startCrepl