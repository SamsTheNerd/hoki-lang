module CoreLang.CoreTyping where
import CoreLang.CoreSorts
import CoreLang.Typad
import Control.Monad ( void, zipWithM_ )
import Data.Map hiding (foldr, map)

---- Sam's ramblings, feel free to ignore :)

-- TConstraint represents a constraint, which currently is just locked to a type (CExact) or floating (CAny), but may more generally represent a family of allowed types. 
-- In haskell we're used to seeing quantified/polymorphic types like `Maybe a` or `[(a,b)]`, where the type is quantified over a given type variable. We generally represent that with `TQuant [tvars] typebody`
-- We're also used to seeing typeclasses as `Constraint tvar => TCon tvar tvar ..`. So it feels natural to want to smush constraints in with quantified typing, since we really only need to explicitly constrain quantified tvars, all other constraints can get simply substituted directly
-- Leaves us at a weird bit where constraints *do* show up in types (atleast top level ones), but also act as type wrappers (especially currently where that's *all* they do). So then the question is, do we treat constraints as just a thing to show up in quantified types, or do we treat every type as a constraint? 
-- I suppose it'd make sense to treat it similarly to how we know to treat higher rank bits already? have constraints sorta floating around in meta vars until we quantify/zonk them out? 

-------- Rambling's over (well, big chunk of rambling is over)

-- this code also borrows heavily from the design given in the paper linked in Typad.hs

-- infers a type at the top level 
inferTypeTL :: Expr -> Typad Type
inferTypeTL expr = do
    mv <- TMetaVar <$> newMetaTVar
    inferTypeTLRec expr mv

-- infers a type at the top level and unifies with given type (used for let recs)
inferTypeTLRec :: Expr -> Type -> Typad Type
inferTypeTLRec expr aty = do
    ty <- inferType expr
    -- lift . putStrLn $ "ty: " ++ show ty
    zty <- zonkType ty
    -- lift . putStrLn $ "zty: " ++ show zty
    zaty <- zonkType aty
    -- lift . putStrLn $ "zaty: " ++ show zaty
    uty <- unifyType zaty zty
    -- lift . putStrLn $ "uty: " ++ show uty
    zuty <- zonkType uty
    -- lift . putStrLn $ "zuty: " ++ show zuty
    quantifyType zuty

-- the bidirectional higher rank paper distinguishes between rho and sigma forms of its typing functions. We'll do that to an extent here but we don't need to consider it *as much* since we're not dealing with higher rank. really just quantifying on tl and making sure to instantiate variable bindings.

inferType :: Expr -> Typad Type
inferType expr = do
    mv <- newMetaTVar
    typeType expr (TInfer mv)
    return $ TMetaVar mv

checkType :: Expr -> Type -> Typad ()
checkType expr ty = typeType expr (TCheck ty)

-- infer type and check type will not be very different for most expressions

-- allows us to write code generically for checking and inferring. trick taken from practical higher rank paper.
data TyExp = TCheck Type -- check against the given type
           | TInfer MetaTVar -- infer and place it in the given metavar location

-- there will be a lot of these functions ! 
typeType :: Expr -> TyExp -> Typad ()

-- nothing fancy should be happening with literals, so just let unify handle it
typeType (ELit lit) expr = void $ instType' (getLiteralType lit) expr

typeType (EPrimOp (PrimOp _ ty _)) expr = void $ instType' ty expr

typeType (EVar v) expr = void $ lookupVarTy v >>= flip instType' expr
-- typeType (EVar v) exp = do

-- infer lambda type by making new meta vars for arg and body and inferring body type in terms of arg type.
typeType (ELambda varg body) (TInfer mv) = do
    argMV <- newMetaTVar -- should this be a type var ? i don't think so ? idk
    bodyMv <- extendVarEnvT varg (TMetaVar argMV) (inferType body)
    -- do we need to handle chaining(?) constraints? does zonking just handle that later? i think so maybe? 
    writeMetaTVar mv (CExact (TArrow (TMetaVar argMV) bodyMv))

-- check lambda type by splitting given type into arg and result types and then checking body type in terms of arg type
typeType (ELambda varg body) (TCheck ty) = do
    -- (lift . putStrLn) $ "inferring type of lambda: " ++ show body
    (argTy, resTy) <- splitFun ty
    extendVarEnvT varg argTy (checkType body resTy)

typeType (EApp fun arg) (TInfer mv) = do
    -- (lift . putStrLn) $ "inferring type of applying " ++ show fun ++ " to " ++ show arg
    fTy <- inferType fun
    argTy <- inferType arg
    void $ unifyType fTy (TArrow argTy (TMetaVar mv))

-- does type constructed stuff Just Work?? appears to. come back here (and review pre-cleanup git history) if this bites us later

typeType (ECase expr cases) expd = do
    exprTy <- inferType expr -- infer our argument
    patBinds <- mapM (bindPats . fst) cases -- infer types of patterns, or atleast their shape
    mapM_ (unifyType exprTy . fst ) patBinds -- unify pattern shapes with argument type 
    -- we don't care about result as much as we care about filling in holes
    let caseBinds = zipWith (\pb cas -> (snd pb, snd cas)) patBinds cases
    mapM_ (\(subst, body) -> extendVarEnvTs subst (typeType body expd)) caseBinds

typeType e _ = typadErr $ "not yet implemented check/inference of " ++ show e


-- creates a type out of the pattern. this will likely be largely full of meta vars 
-- but gives us a type structure to work with
bindPats :: Pattern -> Typad (Type, Map Ident Type)
bindPats PAny = do mv <- newMetaTVar; return (TMetaVar mv, empty)
bindPats (PVar vid) = do
    mdcl <- lookupDC' vid 
    case mdcl of
        Nothing -> do 
            mv <- newMetaTVar; let tMv = TMetaVar mv;
            return (tMv, fromList [(vid, tMv)])
        (Just (DataCons dcId _, _)) -> bindPats (PCons dcId [])
bindPats (PLabel lbl inrPat) = do
    (inrTy, inrSubst) <- bindPats inrPat
    return (inrTy, insert lbl inrTy inrSubst)
bindPats (PCons dcId pats) = do
    inrBinds <- mapM bindPats pats -- bind the inner patterns
    let fullSubst = foldr (\x acc -> snd x <> acc) empty inrBinds -- combine their subst maps
    let inrTys = map fst inrBinds -- grab the tl types out
    tc <- unifyCons dcId inrTys
    return (tc, fullSubst) -- construct a dummy cons type that represents this pattern.
bindPats (PLit expr) = do
    ty <- inferType expr
    return (ty, empty)

-- get the type (tcon) of an abstract constructed data
-- ex: Just Int would give List Int
-- ex: unifyCons "ListCons" [Int, List Int] == List Int
unifyCons :: Ident -> [Type] -> Typad Type
unifyCons dcId instArgs = do
    (DataCons _ dcArgs, TypeCons tcId tvs _) <- lookupDC dcId
    mvs <- mapM (const $ TMetaVar <$> newMetaTVar) tvs -- a meta var for each type cons tvar
    let subst = fromList $ zip tvs mvs
    let sArgs = map (substType subst) dcArgs -- replace tvars with placeholders in arg types
    zipWithM_ unifyType instArgs sArgs -- unify instance args with general args
    return $ TCon tcId mvs

-- need a better name for this, but it does instType and handles unwrapping Expected
instType' :: Type -> TyExp -> Typad Type
instType' ty (TCheck cty) = unifyType ty cty
instType' ty (TInfer mv) = do
    -- (lift . putStrLn) $ "instantiating type' " ++ show ty
    tcon <- instType ty
    writeMetaTVar mv (CExact tcon)
    -- (lift . putStrLn) $ "done instantiating type' " ++ show ty ++ "; got: " ++ show tcon
    return $ TMetaVar mv