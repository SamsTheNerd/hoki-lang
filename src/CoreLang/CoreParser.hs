module CoreLang.CoreParser where
import Text.Parsec.String
import CoreLang.CoreSorts
import Text.Parsec.Error
import Text.Parsec

-- mostly just to test core lang

-- parse an int
intP :: Parser Expr
intP = ELit . LInt . read <$> many1 digit

-- -- parse a boolean
-- boolP :: Parser Expr
-- boolP = (EPrim . PBool) True <$ string "True" <|>
--             (EPrim . PBool) False <$ string "False"

-- parse a lambda
lambdaP :: Parser Expr
lambdaP = do
    char '\\' -- start with backslash
    v <- lower; vs <- many alphaNum -- parse the var
    spaces; string "->"; spaces -- have the function arrow
    ELambda (v:vs) <$> exprP -- parse the body and make the lambda

-- parse a variable
varP :: Parser Expr
varP = do v <- lower; vs <- many alphaNum; return $ EVar (v:vs)

-- parse an application
appP :: Parser Expr
appP = do char '('; fn <- exprP; skipMany1 space; res <- EApp fn <$> exprP; char ')'; return res

-- parse parenthesis
wrapParens :: Parser a -> Parser a
wrapParens p = do char '('; spaces; res <- p; spaces; char ')'; return res

-- parse an expression
exprP :: Parser Expr
exprP = try varP <|> try intP <|> try lambdaP <|> try appP <|> wrapParens exprP

fullExprP = do e <- exprP; eof; return e

parseExpr :: String -> Either ParseError Expr
parseExpr = parse fullExprP  ""

parseExpr' str = case parseExpr str of
    Right exp -> exp

prettyExpr :: Expr -> String
prettyExpr (EVar v) = v
prettyExpr (ELambda x body) = "\\" ++ x ++ " -> " ++ prettyExpr body
prettyExpr (EApp fn arg) = "(" ++ prettyExpr fn ++ " " ++ prettyExpr arg ++ ")"
prettyExpr (ELit (LInt x)) = show x
-- prettyExpr (EPrim (PBool x)) = show x