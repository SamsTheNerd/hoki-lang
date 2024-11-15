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

exprLitP :: Parser Expr
exprLitP = try intP

-- parse a lambda
lambdaP :: Parser Expr
lambdaP = do
    char '\\' -- start with backslash
    v <- lower; vs <- many alphaNum -- parse the var
    spaces; string "->"; spaces -- have the function arrow
    ELambda (v:vs) <$> exprP -- parse the body and make the lambda

identP :: Parser String
identP = do v <- lower; vs <- many alphaNum; return (v:vs)

-- parse a variable
varP :: Parser Expr
varP = EVar <$> identP

-- parse an application
appP :: Parser Expr
appP = do char '('; fn <- exprP; skipMany1 space; res <- EApp fn <$> exprP; char ')'; return res

-- parse parenthesis
wrapParens :: Parser a -> Parser a
wrapParens p = do char '('; spaces; res <- p; spaces; char ')'; return res

-- parse an expression
exprP :: Parser Expr
exprP = try varP <|> try exprLitP <|> try lambdaP <|> try appP <|> wrapParens exprP

-- type parsers

tVarP :: Parser Type
tVarP = TVar <$> identP

tArrowP :: Parser Type
tArrowP = do
    -- char '('
    fromT <- typeButNotArrowP
    spaces
    string "->"
    spaces
    toT <- typeP 
    -- char ')'
    return $ TArrow fromT toT
    -- chainl1 typeP ( TArrow <$ string "->")


tQuantP :: Parser Type
tQuantP = do
    string "forall"
    tvars <- many1 (try $ do spaces; identP)
    spaces
    string "=>"
    spaces
    TQuant ((,CAny)<$> tvars) <$> typeP

tConP :: Parser Type
tConP = do 
    name <- identP
    args <- many (spaces >> typeP)
    return $ TCon name args

typeButNotArrowP :: Parser Type
typeButNotArrowP = try tQuantP <|> try tConP <|> try tVarP

typeP :: Parser Type
typeP = try tArrowP <|> try typeButNotArrowP

-- pattern parsers

patLitP :: Parser Pattern
patLitP = PLit <$> exprLitP

patAnyP :: Parser Pattern
patAnyP = PAny <$ char '_'

patVarP :: Parser Pattern
patVarP = PVar <$> identP

patLabelP :: Parser Pattern
patLabelP = do
    lab <- identP
    char '@'
    PLabel lab <$> patternP

patConsP :: Parser Pattern
patConsP = do
    char '('
    spaces
    name <- identP
    spaces
    pats <- many1 patternP 
    spaces
    char ')'
    return $ PCons name pats


patternP :: Parser Pattern
patternP = try patLitP <|> try patAnyP <|> try patLabelP <|> try patConsP <|> patVarP

-- type cons stuffs

-- dataconsP = do
--     name <- identP


fullExprP = do e <- exprP; eof; return e

parseExpr :: String -> Either ParseError Expr
parseExpr = parse fullExprP  ""

parseExpr' str = case parseExpr str of
    Right exp -> exp