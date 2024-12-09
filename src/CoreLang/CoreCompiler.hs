module CoreLang.CoreCompiler(
    compileExpr,
    haskellifyExpr,
    haskellifyProg
) where
import CoreLang.CoreSorts
import CoreLang.CoreLoader (allPrimOps)



compileExpr prog outpath = undefined

haskellifyProg :: Program -> String
haskellifyProg = foldMap ((++ "\n\n") . haskellifyStmt)

haskellifyStmt :: Statement -> String
haskellifyStmt (SLetRec name expr mAnnot) = name ++ " = " ++ haskellifyExpr expr
haskellifyStmt (STypeDef (TypeCons tid tvs (dc:dcs))) = "data " ++ tid 
    ++ foldMap ((" "++)) tvs ++ " = " 
    ++ foldr ((++) . (++ " | ") . haskellifyDC) (haskellifyDC dc) dcs

haskellifyDC (DataCons did tys) = did ++ " " ++ foldMap ((\s -> " (" ++ s ++ ") ") . haskellifyType) tys

haskellifyType :: Type -> String
haskellifyType (TVar tyid) = tyid
haskellifyType (TNamed tname) = tname
haskellifyType (TArrow fromT toT) = "( " ++ haskellifyType fromT ++ " ) -> ( "
    ++ haskellifyType toT ++ " ) "
haskellifyType (TQuant vs ty) = haskellifyType ty
haskellifyType (TMetaVar _) = "[shouldn't have metavars]"
haskellifyType (TCon tid tys) = " " ++ tid 
    ++ foldMap ((\s -> " (" ++ s ++ ") ") . haskellifyType) tys ++ " "
    

haskellifyExpr :: Expr -> String
haskellifyExpr (EPrimOp (PrimOp _ _ hsprim)) = hsprim
haskellifyExpr (EVar vid) = case lookup vid allPrimOps of
    (Just primOp) -> haskellifyExpr primOp
    Nothing -> vid
haskellifyExpr (ELambda arg body) = "\\" ++ arg ++ " -> " ++ haskellifyExpr body
haskellifyExpr (EApp func arg) = " ( (" ++ haskellifyExpr func ++ " ) ( "
    ++ haskellifyExpr arg ++ ") )"
haskellifyExpr (ELit (LInt i)) = show i
haskellifyExpr (ELit (LDouble d)) = show d
haskellifyExpr (ELit (LChar c)) = show c
haskellifyExpr (ECons cname args) = cname ++ foldMap (\e -> " ( " ++ haskellifyExpr e ++ " ) ") args
haskellifyExpr expr = "[not yet implemented]"