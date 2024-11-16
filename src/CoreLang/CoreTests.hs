module CoreLang.CoreTests where
import CoreLang.CoreParser
import CoreLang.CoreEval
import CoreLang.CoreSorts
import Data.Bifoldable (Bifoldable(bifoldMap))
import CoreLang.PrimLoader (evalProgram)

-- eval tests

evalTests :: [(String, String)]
evalTests = [
    ("\\x -> x", "\\x -> x"),
    ("(\\x -> x a)", "a"),
    ("((\\x -> \\y -> x 1) 2)", "1"), -- true !
    ("((\\x -> \\y -> y 1) 2)", "2"), -- false !
    -- ("\\x -> \\y -> \\z -> ((x z) (y z))", "")
    ("((\\f -> \\g -> \\x -> (f (g x)) \\x -> x) \\x -> x)", "\\x -> x")
    ]

testEval :: (String,String) -> IO ()
testEval (str,exp) =
    do
        res <- resIO
        putStr $ if res == exp then "\ESC[1;32m+ " else if exp == "" then "\ESC[1;35m* " else "\ESC[1;31m- "
        putStr $ exprPretty
        putStr " ~> "
        putStrLn $ res ++ "\ESC[0m"
        return ()
    where expr' = parseExpr str
          exprPretty = case expr' of
            (Left err) -> "parse error"
            (Right expr) -> show expr
          resIO = case expr' of 
            (Left err) -> (return . show) err
            -- (Right expr) -> either show id <$> fullEval expr
            (Right expr) -> foldMap show <$> fullEval expr

runEvalTests :: IO ()
runEvalTests = mapM_ testEval evalTests

runFile :: FilePath -> Expr -> IO (Either String Expr)
runFile fn expr = do
    progOrErr <- readProgramFile fn
    case progOrErr of
        (Right prog) -> do 
            res <- evalProgram prog expr
            return $ case res of
                (Left rExp) -> Left rExp
                (Right err) -> Right err
        (Left err) -> (return . Left . show) err

-- loadFile :: FilePath -> Expr -> IO (Either String Expr)
loadFile fn  = do
    bifoldMap show show <$> readProgramFile fn
    -- case progOrErr of
    --     (Right prog) -> do 
    --         res <- evalProgram prog expr
    --         return $ case res of
    --             (Left rExp) -> Left rExp
    --             (Right err) -> Right err
    --     (Left err) -> (return . Left . show) err