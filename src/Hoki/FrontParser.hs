module Hoki.FrontParser where
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Error
import Text.Parsec
import Hoki.FrontSorts
import Hoki.FrontUtil
import Data.Functor (($>), (<&>))

parseFile :: String -> IO (Either ParseError Program)
parseFile fname = parse programP "" . trim <$> readFile fname

parseExpr :: String -> Either ParseError Expr
parseExpr expr = parse exprP "" expr

testFile :: String -> IO ()
testFile fname = do ep <- parseFile fname
                    case ep of 
                        (Left _) -> return ()
                        (Right prog) -> print prog
--literals

nat :: Parser Integer
nat = do digits <- many1 digit
         return $ read digits

integralP :: Parser Integer
integralP = do char '-' 
               negate <$> nat
            <|> nat

decimalP :: Parser Double
decimalP = try  $  do left <- integralP
                      char '.' 
                      right <- option 0 nat
                      return $ read (show left ++ '.' : show right)
               <|> do left <- option 0 nat
                      char '.' 
                      right <- nat
                      return $ read $ show left ++ '.' : show right
               <|> do string "π" <|> string "pi"
                      return pi
               <|> do string "e"
                      return $ exp 1
                      
whitespace :: Parser Char
whitespace = oneOf " \t"

wsP :: Parser [Char]
wsP = many whitespace

wsP' :: Parser [Char]
wsP' = many1 whitespace

charP :: Parser Char
charP = between (char '\'') (char '\'') 
    (
            try $ noneOf "'\\"
        <|> do char '\\'
               anyChar
    )

stringP :: Parser String
stringP = between (char '"') (char '"') $ many1 
      (
             try $ noneOf "\\\""
        <|> do char '\\'
               anyChar
      )


boolP :: Parser Bool
boolP =     try (
                   do string "true"
                      return True
                )
        <|> try (
                   do string "false"
                      return False
                )          

testP :: Parser [Integer]
testP = do many <- manyTill iP $ lookAhead $ char 'a'
           c <- char 'a'
           return many
        where iP = do {wsP; integralP}


litP :: Parser Literal
litP =  try $  (LDec  <$> decimalP)
           <|> (LInt  <$> integralP)
           <|> (LChar <$> charP)
           <|> (LStr  <$> stringP)
           <|> (LBool <$> boolP)
        



keywords :: Parser Ident
keywords = foldr1 (<|>) $ map string
           [
            "<-", "->", "<><", "<>", "><", "<>[<", "<>]<",
             "+",  "-",   "×",  "÷",  "=",    ">",    "<",
            "<<", "<<",   "∠",  "⦤",  "ⁿ",    "^",    "|",
             "!", "pi"
           ]


varP :: Parser Ident
varP = do lookAhead $ notFollowedBy keywords
          wsP
          pre <- noneOf "1234567890'\"\\ \t\n\r;\0"
          var <- many $ noneOf " \\\t\n\r\0;"
          return $ pre:var
          

abbP :: Parser Expr
abbP = do args <- many1 $ do {v <- varP; wsP'; return v}
          string "<-"
          wsP'
          body <- sepBy1 exprP wsP'
          return $ Eabb (last args) (init args) body Nothing

--funcP :: Parser Ident
--funcP = foldr1 (<|>) $ map string
--           [
--            "<-", "->", "<><", "<>", "><", "<>[<", "<>]<",
--             "+",  "-",   "×",  "÷",  "=",    ">",    "<",
--            "<<", "<<",   "∠",  "⦤",  "ⁿ"
--           ]

funcP :: Parser Ident
funcP = foldr1 (<|>) $ map (foldr1 (<|>) . map string) 
    [
        ["<-"],
        ["->"],
        ["<><","cons"],
        ["<>","encap"],
        ["><","append"],
        ["<>[<","tail"],
        ["<>]<","head"],
        ["+","add"],
        ["-","sub","subtract"],
        ["×","mult","multiply"],
        ["÷","div"],
        ["="],
        [">"],
        ["<"],
        ["<<","fold"],
        [">>","map"],
        ["∠","cos"], 
        ["⦤","acos","arccos"],
        ["ⁿ","pow"]
    ]


argP :: Int -> Parser [Args]
argP n = many (do {a <- (Alit <$> litP) <|> Avar <$> varP; wsP'; return a})

appP :: Parser Expr
appP = do args <- argP 1
          f <- funcP
          return $ Eapp f args  

exprP :: Parser Expr
exprP = try  $  abbP
            <|> appP
            <|> Elit <$> litP
            <|> Evar <$> varP

line :: Parser String
line = do wsP
          option "" $ string "\r"
          string "\n" <|> string ";"
          wsP

lines' = do {line; many $ line <|> wsP'}


programP :: Parser Program
programP = Prog <$> do e <- exprP
                       es <- option [] $ many
                                       (
                                          do lines'
                                             exprP
                                       )
                       option [] lines'
                       eof
                       return $ e:es
