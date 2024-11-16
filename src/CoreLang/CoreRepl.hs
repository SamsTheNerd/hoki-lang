module CoreLang.CoreRepl where
import Control.Monad.State (StateT (..), lift, put, get, MonadIO (liftIO), gets, withStateT)
import CoreLang.CoreSorts
import CoreLang.CoreParser (readProgramFile, parseExpr)
import CoreLang.PrimLoader (evalProgram)
import Data.Bifunctor (Bifunctor(..))
import Control.Monad (void)

-- Core Repl Monad
type CReplad a = StateT (FilePath, Program) IO a

creplDispatch :: Char -> String -> CReplad ()
creplDispatch 'l' arg = do
    errOrProg <- lift $ readProgramFile arg
    case errOrProg of
        (Left err) -> lift $ putStrLn ("error: " ++ show err)
        (Right prog) -> put (arg, prog) >> lift (putStrLn $ "loaded file: " ++ arg)
creplDispatch 'r' _ = do
    (fn, oldP) <- get
    errOrProg <- lift $ readProgramFile fn
    case errOrProg of
        (Left err) -> lift $ putStrLn ("error: " ++ show err)
        (Right prog) -> put (fn, prog) >> lift (putStrLn $ "reloaded file: " ++ fn)

creplEval :: String -> Program -> IO ()
creplEval inp prog = case parseExpr inp of
    (Left err) -> putStrLn ("parse error: " ++ show err)
    (Right expr) -> do
        res <- evalProgram prog expr
        case res of
            (Left err) -> putStrLn ("error: " ++ err)
            (Right resVal) -> print resVal
    -- do
    -- expr <- ()
    -- errOrRes <- evalProgram prog expr


creplLoop :: CReplad ()
creplLoop = do
    lift $ putStr "<3: "
    inp <- lift getLine
    case inp of
        (':':c:' ':arg) -> creplDispatch c arg
        _ -> gets snd >>= lift . creplEval inp
    creplLoop

startCrepl = do
    void $ runStateT creplLoop ("", [])

main = startCrepl