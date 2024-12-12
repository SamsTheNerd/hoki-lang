module CoreLang.CoreRepl where
import Control.Monad.State.Strict (StateT (..), lift, put, get, MonadIO (liftIO), gets)
import CoreLang.CoreSorts
import CoreLang.CoreParser (readProgramFile, parseExpr, parseType)
import CoreLang.CoreLoader (evalProgram, inferInProgram, LProg (LProg), loadProgram, coreProg, runTypadInProgram)
import System.Console.Haskeline
import Data.Maybe (fromMaybe)
import Data.Map (keys)
import CoreLang.CoreCompiler (haskellifyExpr, haskellifyProg)
import CoreLang.Typad (getKind)

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
creplDispatch "k" ty = creplGetKind ty
creplDispatch "h" expr = creplAct (\_ -> return . haskellifyExpr) expr
creplDispatch "hl" _ = do
    mayFP <- lift (gets fst)
    mrrp <- liftIO $ maybe (return $ Right []) readProgramFile mayFP
    outputStrLn $ either show haskellifyProg mrrp 
creplDispatch "tAll" _ = lift (gets snd) >>= \(LProg _ _ _ venv) -> mapM_ creplInferType (keys venv)
creplDispatch "help" _ = outputStrLn $ "\nWelcome to the crepl! (core repl) \n\
                                       \You can enter expressions to evaluate them in the context of the currently loaded program or use one of the following commands:\n\n\
                                       \:help         -- display this help screen\n\
                                       \:l <filepath> -- load the given core-lang file\n\
                                       \:r            -- reload the currently loaded core-lang file\n\
                                       \:t <expr>     -- infers the type of the given expression\n\
                                       \:tAll         -- displays the inferred types of all variables in the current environment\n"
creplDispatch cmd _ = outputStrLn $ "unknown command :"  ++ cmd ++ "\n\tuse :help to see all available commands"

creplInferType :: String -> CReplad ()
creplInferType expr = creplAct ((\case
            (Left err) -> ("error: " ++ err)
            (Right resVal) -> show expr ++ " :: " ++ show resVal)
        <$:> inferInProgram) expr

creplGetKind :: String -> CReplad ()
creplGetKind inp = case parseType inp of
    (Left err) -> outputStrLn ("parse error: " ++ show err)
    (Right ty) -> do
        lprog <- lift (gets snd)
        liftIO (runTypadInProgram lprog $ getKind ty) >>= (outputStrLn . either ("type error: " ++) show)

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