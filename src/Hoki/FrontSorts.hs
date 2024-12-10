module Hoki.FrontSorts where

type Ident = String
data Type = Ident

-- data Expr 
data Expr = Evar Ident
          | Elit Literal
          | Eapp Ident [Args]
          | Eabb Ident [Ident] [Expr] (Maybe Type)
          | Econs Ident

data Args = Avar Ident
          | Alit Literal

data Literal = LDec  Double
             | LInt  Integer
             | LChar Char
             | LStr  String
             | LBool Bool
             deriving (Show, Eq)

instance Show Expr where
    show (Evar v) = v
    show (Elit l) = show l
