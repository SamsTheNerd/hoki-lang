module CoreLang.CoreRepl where
import Control.Monad.State.Strict (StateT (..), lift, put, get, MonadIO (liftIO), gets, withStateT)
import CoreLang.CoreSorts
import CoreLang.CoreParser (readProgramFile, parseExpr)
import CoreLang.CoreLoader (evalProgram, inferInProgram, LProg, loadProgram, emptyLProg)
import Data.Bifunctor (Bifunctor(..))
import Control.Monad (void)
import System.Console.Haskeline
import Data.Maybe (fromMaybe)
import CoreLang.CoreTyping (inferType, inferTypeTL)
import CoreLang.Typad (TypadST(TypadST), runTypad)
import Data.Map (empty)
import Data.IORef (newIORef)

-- Core Repl Monad
type CReplad a = InputT (StateT (Maybe (FilePath, LProg)) IO) a

creplDispatch :: Char -> String -> CReplad ()
creplDispatch 'l' arg = do
    errOrProg <- liftIO $ readProgramFile arg
    errOrLProg <- case errOrProg of
        (Left err) -> liftIO . return . Left $  ("error: " ++ show err)
        (Right prog) -> liftIO $ loadProgram prog
    case errOrLProg of
        (Left err) -> liftIO . putStrLn $ "error: " ++ show err
        (Right lprog) -> (lift . put . Just $ (arg, lprog)) >> liftIO (putStrLn $ "loaded file: " ++ arg)
creplDispatch 'r' _ =lift get >>= \case
    (Just (fn, _)) -> do
        errOrProg <- liftIO $ readProgramFile fn
        errOrLProg <- case errOrProg of
            (Left err) -> liftIO . return . Left $  ("error: " ++ show err)
            (Right prog) -> liftIO $ loadProgram prog
        case errOrLProg of
            (Left err) -> liftIO . putStrLn $ "error: " ++ show err
            (Right lprog) -> (lift . put . Just $ (fn, lprog)) >> liftIO (putStrLn $ "loaded file: " ++ fn)
    Nothing -> outputStrLn "no program loaded"
creplDispatch 't' expr = lift (gets (maybe emptyLProg snd)) >>= creplInferType expr 
creplDispatch cmd arg = outputStrLn $ "unknown command :"  ++ [cmd]


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
        (':':c:' ':arg) -> creplDispatch c arg -- TODO: this is disgusting
        (':':c:arg) -> creplDispatch c arg
        "" -> return ()
        _ ->  lift (gets (maybe emptyLProg snd)) >>= creplEval inp
    creplLoop

startCrepl :: IO ()
startCrepl = void $ runStateT (runInputT defaultSettings creplLoop) Nothing

-- main = startCrepl