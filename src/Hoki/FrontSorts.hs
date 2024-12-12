
module Hoki.FrontSorts where

import Hoki.FrontUtil

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
          deriving(Eq)


data Literal = LDec  Double
             | LInt  Integer
             | LChar Char
             | LStr  String
             | LBool Bool
             deriving (Eq)

newtype Program = Prog [Expr] deriving (Show)

instance Show Literal where
    show (LDec d)  = show d 
    show (LInt i)  = show i
    show (LChar c) = show c
    show (LStr s)  = show s
    show (LBool b) = show b

instance Show Args where
    show (Avar a) = a
    show (Alit l) = show l



instance Show Expr where
    show (Evar v) = "Evar" ++ v
    show (Elit l) = show l
    show (Eabb n a b t) = "(" ++ joinWith' " " a ++ n ++ " <- " ++  joinWith "; " (map show b) ++ ")"
    show (Eapp i a) = "(" ++ joinWith " " (map show  a) ++ ") " ++ i




