module Hoki.FrontSorts where

import Hoki.FrontUtil
import Distribution.Simple.Program (Program)

type Ident = String

-- data Expr 
data Expr = Evar Ident 
          | Elit Literal
          | Eapp Ident [Args]
          | Eabb Ident [Ident] [Expr] (Maybe Type)
          | Econs Ident 

data Type = Tsig Ident | Tabb [Type] Type

data Args = Avar Ident
          | Alit Literal
          deriving(Show, Eq)

data Literal = LDec  Double
             | LInt  Integer
             | LChar Char
             | LStr  String
             | LBool Bool
             deriving (Show, Eq)

newtype Program = Prog [Expr] deriving (Show)

instance Show Expr where
    show (Evar v) = "Evar" ++ v
    show (Elit l) = show l
    show (Eabb n a b t) = "(" ++ joinWith' " " a ++ n ++ " <- " ++  joinWith "; " (map show b) ++ ")"
