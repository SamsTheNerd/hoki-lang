module CoreLang.CoreParser where
import Text.Parsec.String
import CoreLang.CoreSorts
import Text.Parsec.Error
import Text.Parsec
import Data.Functor (($>))

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
    v <- identP
    bSpaces; string "->"; bSpaces; -- have the function arrow
    ELambda v <$> exprP -- parse the body and make the lambda

identP :: Parser String
-- identP = do v <- lower; vs <- many alphaNum; return (v:vs)
identP = do v <- letter; vs <- many alphaNum; return (v:vs)

-- parse a variable
varP :: Parser Expr
varP = EVar <$> identP

-- parse an application
appP :: Parser Expr
appP = do char '('; fn <- exprP; bSpaces1; res <- EApp fn <$> exprP; char ')'; return res

eCaseP :: Parser Expr
eCaseP = do
    string "case"; bSpaces1
    exp <- exprP; bSpaces1
    string "of";
    cases <- (many1 . try ) $ do
        bSpaces1
        pat <- patternP; bSpaces
        string "->"; bSpaces
        fun <- exprP; 
        -- skipMany1 (char ';')
        optional $ char ';'
        return (pat, fun)
    return $ ECase exp cases

-- parse parenthesis
wrapParens :: Parser a -> Parser a
wrapParens p = do char '('; bSpaces; res <- p; bSpaces; char ')'; return res

-- parse an expression
exprP :: Parser Expr
exprP = try exprLitP <|> try lambdaP <|> try appP <|> eCaseP <|> try varP <|> wrapParens exprP

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
typeButNotArrowP = try tQuantP <|> try tConP <|> try tVarP <|> wrapParens typeButNotArrowP

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
    bSpaces
    name <- identP
    bSpaces
    pats <- (many1 .try) (bSpaces >> patternP)
    bSpaces
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

-- typedef parsing

dataconP :: Parser DataCons
dataconP = do
    name <- identP
    args <- many (try $ bSpaces >> typeP)
    -- bSpaces
    -- args <- chainl ((:[]) <$> typeP) ((<>) <$ try bSpaces) []
    -- bSpaces
    return $ DataCons name args

typeconsP :: Parser TypeCons
typeconsP = do
    string "data " -- yea why not, why is gonna look like haskell anyways
    name <- identP
    gens <- many (try $ bSpaces >> identP)
    bSpaces
    char '='
    bSpaces
    dcs <- chainl1 ((:[]) <$> dataconP) ((<>) <$ try (do bSpaces; char '|'; bSpaces))
    char ';'
    return $ TypeCons name gens dcs

-- dealing with parsing whole files now

tlLetP :: Parser Statement
tlLetP = do
    string "let"
    bSpaces
    name <- identP 
    bSpaces 
    annot <- (do
        string "::"
        spaces
        Just <$> typeP) <|> return Nothing
    bSpaces
    char '='
    bSpaces
    exp <- exprP
    return $ SLetRec name exp annot


programP :: Parser Program
programP = do 
    bSpaces
    chainl ((:[]) <$> (
        try tlLetP 
        <|> (STypeDef <$> typeconsP))
        <|> ([] <$ try (char '#' >> many (noneOf ['\n', '\r'])) ) -- comments!
        <|> ([] <$ eof ) -- end!
        ) ((<>) <$ try bSpaces1) []

readProgramFile :: FilePath -> IO (Either ParseError Program)
readProgramFile fn = parseFromFile programP fn

-- breakable spacing

bSpaces_ :: Parser Char
bSpaces_ = (char '#' >> many (noneOf ['\n', '\r']) $> ' ') -- comments!
    -- can have new endOfLines but they must be followed by an indent (TODO: only on scope change?)
    <|> try (endOfLine >> tab)
    <|> try (endOfLine >> (' ' <$ many1 space))
    <|> tab 
    <|> space
    

bSpaces :: Parser ()
bSpaces = skipMany bSpaces_

bSpaces1 :: Parser ()
bSpaces1 = skipMany1 bSpaces_