module CoreLang.CoreRepl where
import Control.Monad.State.Strict (StateT (..), lift, put, get, MonadIO (liftIO), gets, withStateT)
import CoreLang.CoreSorts
import CoreLang.CoreParser (readProgramFile, parseExpr)
import CoreLang.PrimLoader (evalProgram)
import Data.Bifunctor (Bifunctor(..))
import Control.Monad (void)
import System.Console.Haskeline
import Data.Maybe (fromMaybe)
import CoreLang.CoreTyping (inferType, inferTypeTL)
import CoreLang.Typad (TypadST(TypadST), runTypad)
import Data.Map (empty)
import Data.IORef (newIORef)

-- Core Repl Monad
type CReplad a = InputT (StateT (FilePath, Program) IO) a

creplDispatch :: Char -> String -> CReplad ()
creplDispatch 'l' arg = do
    errOrProg <- liftIO $ readProgramFile arg
    case errOrProg of
        (Left err) -> liftIO $ putStrLn ("error: " ++ show err)
        (Right prog) -> lift $ put (arg, prog) >> liftIO (putStrLn $ "loaded file: " ++ arg)
creplDispatch 'r' _ = do
    (fn, oldP) <- lift get
    errOrProg <- liftIO $ readProgramFile fn
    case errOrProg of
        (Left err) -> outputStrLn ("error: " ++ show err)
        (Right prog) -> lift $ put (fn, prog) >> liftIO (putStrLn $ "reloaded file: " ++ fn)
creplDispatch 't' expr = creplInferType expr undefined
    
creplInferType :: String -> Program -> CReplad ()
creplInferType inp prog = case parseExpr inp of
    (Left err) -> outputStrLn ("parse error: " ++ show err)
    (Right expr) -> do
        fref <- liftIO $ newIORef 0 
        res <- liftIO $ runTypad (inferTypeTL expr) (TypadST empty empty fref)
        -- res <- liftIO $ evalProgram prog expr
        case res of
            (Left err) -> outputStrLn ("error: " ++ err)
            (Right resVal) -> outputStrLn $ show expr ++ " :: " ++ show resVal

creplEval :: String -> Program -> CReplad ()
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
        _ ->  lift (gets snd) >>= creplEval inp
    creplLoop

startCrepl :: IO ()
startCrepl = void $ runStateT (runInputT defaultSettings creplLoop) ("", [])

-- main = startCrepl