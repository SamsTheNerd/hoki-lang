module CoreLang.CoreSorts where
import Data.Map (Map)
import Data.IORef (IORef)
import qualified Data.Map
import Data.Maybe (fromMaybe)
import Data.List (nub, (\\))

-- loosely based on Data.hs from ps7 to start with
type Ident = String -- maybe want something More here?

data Expr = EVar Ident -- variable, defined by substitution
          | ELambda Ident Expr --- lambda abstraction of var over an expression
          | EApp Expr Expr -- application (e1 e2) in that order
          | ECase Expr [(Pattern, Expr)] -- a case expression. runs down the list of patterns, finds the first that matches, and runs the expression with the appropriate variables bound
          | ECons Ident [Expr] -- a data expression, should be fully instantiated here, otherwise it should be a series of lambdas
          | ELit Literal -- a primitive literal
          | EPrimOp PrimOp-- primitive low level functions 


type TVar = String

data Type = TVar TVar
          | TNamed Ident -- can this be merged in with constructed types? probably not ?
          | TArrow Type Type
          | TQuant [(TVar, TConstraint)] Type
          | TMetaVar MetaTVar
        --   | TCon TypeCons [Type] -- a constructed type - should generally be fully instantiated, so kind *, but may be instantiated with placeholder typevars. TBD how to handle it if/when we get to typeclasses
          | TCon Ident [Type] -- a constructed type - should generally be fully instantiated, so kind *, but may be instantiated with placeholder typevars. TBD how to handle it if/when we get to typeclasses
        deriving (Eq)

-- a (possibly higher kinded) type constructor 
data TypeCons = TypeCons Ident [TVar] [DataCons] -- 
    deriving (Show, Eq)

data DataCons = DataCons Ident [Type]-- a data constructor is made from a series of types, possibly higher kinded.
    deriving (Show, Eq)

-- constraints for typing. consider these semi-internal for the time being
data TConstraint = CExact Type -- an exact type
                 | CAny 
                --  | TClass -- typeclasses in the future?
                deriving (Show, Eq)

data MetaTVar = MetaTVar Int (IORef TConstraint) -- a meta variable used to build up found constraints
              | MetaStrVar String (IORef TConstraint) -- a hack to make letrec the tiniest bit easier
    deriving (Eq)

-- language primitives (would be nice to have a way to introduce new prims easily? TODO: decide how exactly to have this, have it be informed by Type constructors?)
data Literal = LInt Int
             | LDouble Double
             | LChar Char
            deriving (Show, Eq)


primIntT :: Type
primIntT = TNamed "PrimInt"

primDoubleT :: Type
primDoubleT = TNamed "PrimDouble"

primCharT :: Type
primCharT = TNamed "PrimChar"

getLiteralType :: Literal -> Type
getLiteralType (LInt _) = primIntT
getLiteralType (LDouble _) = primDoubleT
getLiteralType (LChar _) = primCharT

-- currently this prevents Expressions from being equality check-able
data PrimOp = PrimOp (Expr -> Runad Expr) Type String -- will add more type checker bits here. -- string is equivalent haskell code for compiling :p

-- a pattern that tries to match an expression 
data Pattern = PLit Expr -- match for equality ? idk if we can actually support that reliably
             | PAny
             | PVar Ident -- matches any expression, binds it to the variable (this could probably be done with just label any)
             | PLabel Ident Pattern -- matches the given pattern, binds it to the variable if matches
             | PCons Ident [Pattern]
    deriving (Show)

data Statement = STypeDef TypeCons 
               | SLetRec  Ident Expr (Maybe Type) -- letrec binding expression to the identifier (with optional type annotation for polymorphic declarations)
               -- TODO: imports ? or have those just flatten down to statements
               deriving (Show)

type Program = [Statement] -- a program is a series of statements in no particular order

-- api/header-y bits end here!

instance Show Expr where
    show (EVar v) = v
    show (ELambda x body) = "\\" ++ x ++ " -> " ++ show body
    show (EApp fn arg) = "(" ++ show fn ++ " " ++ show arg ++ ")"
    show (ELit (LInt x)) = show x
    show (ECons dcid expr) = dcid ++ "#" ++ show expr
    show (ECase inp pats) = "case[" ++ show inp ++ "]{"
        ++ foldr (\(p, f) str -> 
            str ++ (if null str then "" else ";") ++"\n\t" ++ show p 
            ++ " |-> " ++ show f) "" pats ++ "\n}"
    show (EPrimOp (PrimOp _ ty _)) = "primOp(~: " ++ show ty ++ ")"
    show (ELit (LDouble x)) = show x
    show (ELit (LChar x)) = show x

instance Show Type where
    show (TVar v) = v
    show (TArrow f@(TArrow _ _) t) = "(" ++ show f ++ ") -> " ++ show t
    show (TArrow f t) = show f ++ " -> " ++ show t
    show (TMetaVar mv) = show mv
    show (TQuant cs body) = "forall" ++
        foldMap ((" "++) . fst) cs ++ " => " ++ show body
    show (TCon tcid holes) = tcid ++ foldMap ((" "++). show) holes
    show (TNamed name) = name

instance Show MetaTVar where
    show (MetaTVar i _) = "meta@" ++ show i
    show (MetaStrVar str _) = "meta@" ++ show str

instance Ord MetaTVar where
    (MetaTVar i _ ) <= (MetaTVar j _) = i <= j
    (MetaStrVar str1 _ ) <= (MetaStrVar str2 _) = str1 <= str2
    (MetaTVar _ _ ) <= (MetaStrVar _ _) = True
    (MetaStrVar _ _ ) <= (MetaTVar _ _) = False

substType :: Map TVar Type -> Type -> Type
substType subs ty@(TVar v) = fromMaybe ty (Data.Map.lookup v subs)
substType subs (TArrow frT toT) = TArrow (substType subs frT) (substType subs toT)
substType subs (TQuant tvs body) = TQuant tvs $ substType (Data.Map.filterWithKey (\k _ -> k `notElem` map fst tvs) subs) body
substType subs (TCon tn ts) = TCon tn $ substType subs <$> ts
substType _ ty = ty

-- replace the metavars with their given types
substMVs :: Map MetaTVar Type -> Type -> Type
substMVs subs ty@(TMetaVar mv) = fromMaybe ty (Data.Map.lookup mv subs)
substMVs subs (TArrow frT toT) = TArrow (substMVs subs frT) (substMVs subs toT)
substMVs subs (TQuant tvs body) = TQuant tvs (substMVs subs body)
substMVs subs (TCon tn ts) = TCon tn $ substMVs subs <$> ts
substMVs _ ty = ty

-- get the free variables in an expression (ofc no zonking here)
getFreeVarsE :: Expr -> [Ident]
getFreeVarsE (EVar vn) = [vn]
getFreeVarsE (ELambda arg body) = getFreeVarsE body \\ [arg]
getFreeVarsE (EApp func arg) = nub $ getFreeVarsE func ++ getFreeVarsE arg
getFreeVarsE (ECons _ es) = nub $ concatMap getFreeVarsE es
-- this def needs better handling for knocking out pattern deps
getFreeVarsE (ECase arg ps) = nub $ getFreeVarsE arg ++ concatMap (\(p, e) -> getFreeVarsE e \\ getBindersP p) ps
getFreeVarsE (ELit _) = []
getFreeVarsE (EPrimOp _) = []

-- get the bound variables of a pattern
getBindersP :: Pattern -> [Ident]
getBindersP PAny = []
getBindersP (PLit _) = []
getBindersP (PLabel name inP) = name:getBindersP inP
getBindersP (PVar name) = [name]
getBindersP (PCons _ ps) = nub $ concatMap getBindersP ps

-- some data types defined here to avoid circular deps

-- runad types, see CoreLang.Runad --

-- a substitution map of variables to expressions
type VarEnv = Map Ident Expr
type DCLookup = Map Ident (DataCons, TypeCons)
type EError = String

-- for info that needs to be pushed down in execution
data RunadSTIn = RunadSTIn VarEnv DCLookup Int 
    deriving (Show)

-- info that needs to come back up in execution
data RunadSTOut = RunadSTOut Int
    deriving (Show)

newtype Runad a = Runad (RunadSTIn -> IO (Either EError a, RunadSTOut))