module CoreLang.CoreSorts where

-- loosely based on Data.hs from ps7 to start with
type Ident = String -- maybe want something More here?

data Expr = EVar Ident -- variable, defined by substitution
          | ELambda Ident Expr --- lambda abstraction of var over an expression
          | EApp Expr Expr -- application (e1 e2) in that order
          | ELit Literal -- a primitive literal
          | ECase [(Pattern, Expr)] -- a case expression. runs down the list of patterns, finds the first that matches, and runs the expression with the appropriate variables bound
          | ECons DataCons [Expr] -- a data expression, may not be fully extantiated ? in which case it has a type TArrow 
    deriving (Show, Eq)

type TVar = String

data Type = TVar TVar
          | TNamed Ident -- can this be merged in with constructed types? probably not ?
          | TArrow Type Type
          | TQuant [(TVar, TConstraint)] Type
          | TMetaVar Int
          | TCon TypeCons [Type] -- a constructed type - should generally be fully instantiated, so kind *, but may be instantiated with placeholder typevars. TBD how to handle it if/when we get to typeclasses
        deriving (Show, Eq)

-- a (possibly higher kinded) type constructor 
data TypeCons = TypeCons Ident [TVar] [DataCons] -- 
    deriving (Show, Eq)

data DataCons = DataCons Ident [Type]-- a data constructor is made from a series of types, possibly higher kinded.
    deriving (Show, Eq)

-- constraints for typing. consider these semi-internal for the time being
data TConstraint = CExact Type 
                 | CAny 
                --  | TClass -- typeclasses in the future?
                deriving (Show, Eq)

-- language primitives (would be nice to have a way to introduce new prims easily? TODO: decide how exactly to have this, have it be informed by Type constructors?)
data Literal = LInt Int
             | LDouble Double
             | LChar Char
            deriving (Show, Eq)

-- a pattern that tries to match an expression 
data Pattern = PLit Expr -- match for equality ? idk if we can actually support that reliably
             | PAny
             | PVar Ident -- matches any expression, binds it to the variable
             | PLabel Ident Pattern -- matches the given pattern, binds it to the variable if matches
             | PCons 
    deriving (Show, Eq)

data Statement = STypeDef TypeCons 
               | SLetRec  Ident Expr (Maybe Type) -- letrec binding expression to the identifier (with optional type annotation for polymorphic declarations)
               -- TODO: imports ?

type Program = [Statement] -- a program is a series of statements in no particular order