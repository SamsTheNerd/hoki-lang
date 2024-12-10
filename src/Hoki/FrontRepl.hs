module Hoki.FrontRepl where
import Control.Monad.State.Strict (StateT (..), lift, put, get, MonadIO (liftIO), gets)
import CoreLang.CoreSorts
import CoreLang.CoreParser (readProgramFile, parseExpr)
import CoreLang.CoreLoader (evalProgram, inferInProgram, LProg (LProg), loadProgram, coreProg)
import System.Console.Haskeline
import Data.Maybe (fromMaybe)
import Data.Map (keys)
import CoreLang.CoreCompiler (haskellifyExpr, haskellifyProg)
import Hoki.FrontParser (exprP, parseExpr)
import InterpLayer.InterpMap (hokiToCoreExpr)

-- Core Repl Monad
type HKreplad a = InputT (StateT (Maybe FilePath, LProg) IO) a

hkreplDispatch :: String -> String -> HKreplad ()
-- hkreplDispatch "l" arg = do
--     errOrProg <- liftIO $ readProgramFile arg
--     errOrLProg <- case errOrProg of
--         (Left err) -> liftIO . return . Left $  ("error: " ++ show err)
--         (Right prog) -> liftIO $ loadProgram prog
--     case errOrLProg of
--         (Left err) -> liftIO . putStrLn $ "error: " ++ show err
--         (Right lprog) -> (lift . put $ (Just arg, lprog)) >> liftIO (putStrLn $ "loaded file: " ++ arg)
-- hkreplDispatch "r" _ =lift get >>= \case
--     (Just fn, _) -> do
--         errOrProg <- liftIO $ readProgramFile fn
--         errOrLProg <- case errOrProg of
--             (Left err) -> liftIO . return . Left $  ("error: " ++ show err)
--             (Right prog) -> liftIO $ loadProgram prog
--         case errOrLProg of
--             (Left err) -> liftIO . putStrLn $ "error: " ++ show err
--             (Right lprog) -> (lift . put $ (Just fn, lprog)) >> liftIO (putStrLn $ "loaded file: " ++ fn)
--     (Nothing, _) -> outputStrLn "no program loaded"
hkreplDispatch "t" expr = hkreplInferType expr
hkreplDispatch "tAll" _ = lift (gets snd) >>= \(LProg _ _ _ venv) -> mapM_ hkreplInferType (keys venv)
hkreplDispatch "help" _ = outputStrLn $ "\nWelcome to the hoki repl \n\
                                       \You can enter expressions to evaluate them in the context of the currently loaded program or use one of the following commands:\n\n\
                                       \:help         -- display this help screen\n\
                                       \:t <expr>     -- infers the type of the given expression\n\
                                       \:tAll         -- displays the inferred types of all variables in the current environment\n"
hkreplDispatch cmd _ = outputStrLn $ "unknown command :"  ++ cmd ++ "\n\tuse :help to see all available commands"

hkreplInferType :: String -> HKreplad ()
hkreplInferType expr = hkreplAct ((\case
            (Left err) -> ("error: " ++ err)
            (Right resVal) -> show expr ++ " :: " ++ show resVal)
        <$:> inferInProgram) expr

hkreplEval :: String -> HKreplad ()
hkreplEval expr = hkreplAct ((\case
            (Left err) -> ("error: " ++ err)
            (Right resVal) -> show resVal)
        <$:> evalProgram) expr

(<$:>) :: Monad m => (c -> d) -> (a -> b -> m c) -> a -> b -> m d
af <$:> f = \x y -> af <$> f x y

hkreplAct :: (LProg -> Expr -> IO String) -> String -> HKreplad ()
hkreplAct f inp = case Hoki.FrontParser.parseExpr inp of
    (Left err) -> outputStrLn ("parse error: " ++ show err)
    (Right expr) -> do
        lprog <- lift (gets snd)
        let cexpr = hokiToCoreExpr lprog expr
        liftIO (f lprog cexpr) >>= outputStrLn

hkreplLoop :: HKreplad ()
hkreplLoop = do
    inp <- fromMaybe "" <$> getInputLine "<3: "
    case inp of
        ":" -> outputStrLn "command expected but none received"
        (':':inps) -> hkreplDispatch cmd (unwords args)
            where (cmd:args) = words inps
        "" -> return ()
        _ ->  hkreplAct ((\case
            (Left err) -> ("error: " ++ err)
            (Right resVal) -> show resVal)
            <$:> evalProgram) inp
    hkreplLoop

starthkrepl :: IO ()
starthkrepl = do
    cprog <- coreProg
    runStateT (runInputT defaultSettings hkreplLoop) (Nothing, cprog)
    return ()