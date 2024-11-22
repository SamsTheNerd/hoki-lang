module CoreLang.CoreRepl where
import Control.Monad.State.Strict (StateT (..), lift, put, get, MonadIO (liftIO), gets, withStateT)
import CoreLang.CoreSorts
import CoreLang.CoreParser (readProgramFile, parseExpr)
import CoreLang.CoreLoader (evalProgram, inferInProgram, LProg (LProg), loadProgram, coreProg)
import Data.Bifunctor (Bifunctor(..))
import Control.Monad (void)
import System.Console.Haskeline
import Data.Maybe (fromMaybe)
import CoreLang.CoreTyping (inferType, inferTypeTL)
import CoreLang.Typad (TypadST(TypadST), runTypad)
import Data.Map (empty, keys)
import Data.IORef (newIORef)

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
creplDispatch "t" expr = lift (gets snd) >>= creplInferType expr
creplDispatch "tAll" _= lift (gets snd) >>= \lprog@(LProg _ _ _ venv) -> mapM_ (`creplInferType` lprog) (keys venv)
creplDispatch cmd arg = outputStrLn $ "unknown command :"  ++ cmd


creplInferType :: String -> LProg -> CReplad ()
creplInferType inp prog = case parseExpr inp of
    (Left err) -> outputStrLn ("parse error: " ++ show err)
    (Right expr) -> do
        res <- liftIO $ inferInProgram prog expr
        -- res <- liftIO $ evalProgram prog expr
        case res of
            (Left err) -> outputStrLn ("error: " ++ err)
            (Right resVal) -> outputStrLn $ show expr ++ " :: " ++ show resVal

creplEval :: String -> LProg -> CReplad ()
creplEval inp prog = case parseExpr inp of
    (Left err) -> outputStrLn ("parse error: " ++ show err)
    (Right expr) -> do
        res <- liftIO $ evalProgram prog expr
        case res of
            (Left err) -> outputStrLn ("error: " ++ err)
            (Right resVal) -> outputStrLn $ show resVal

creplLoop :: CReplad ()
creplLoop = do
    -- liftIO $ putStr "<3: "
    inp <- fromMaybe "" <$> getInputLine "<3: "
    case inp of
        ":" -> outputStrLn "command expected but none received"
        (':':inps) -> creplDispatch cmd (unwords args)
            where (cmd:args) = words inps
        "" -> return ()
        _ ->  lift (gets snd) >>= creplEval inp
    creplLoop

startCrepl :: IO ()
startCrepl = do
    cprog <- coreProg
    runStateT (runInputT defaultSettings creplLoop) (Nothing, cprog)
    return ()

-- main = startCrepl