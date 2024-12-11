module InterpLayer.InterpSetup where
import CoreLang.CoreLoader (LProg, loadProgram, loadPrimOps)
import CoreLang.CoreSorts (Program, TypeCons (..), DataCons (..), Statement (..), Expr (..), Type (..), primIntT, primDoubleT, Pattern (..))

-- this should load in whatever primitive operations or types are needed. 
-- assume core Prims are here already
-- it'd be nice if this was actually written in Hoki but I don't think the parser is quite there yet, see resources/hokibaselib/core for how i'm doing this
hokiPrimProg :: Program
hokiPrimProg = [    
    -- boolean def
    STypeDef (TypeCons "Bool" [] [DataCons "True" [], DataCons "False" []]),
    -- list def
    STypeDef (TypeCons "List" ["a"] [DataCons "Cons" [TVar "a", TCon "List" [TVar "a"]], DataCons "Empty" []]),
    -- number def
    STypeDef (TypeCons "Num" [] [DataCons "NumInt" [primIntT], DataCons "NumDouble" [primDoubleT]]),

    -- numerics
    SLetRec "_add" (ELambda ("x") (ELambda ("y") (ECase (EApp (EApp (EVar "Pair") (EVar "x")) (EVar "y")) [(PCons "Pair" ([PCons "NumInt" ([PVar "i"]), PCons "NumInt" ([PVar "j"])]), EApp (EVar "NumInt") (EApp (EApp (EVar "intAdd") (EVar "i")) (EVar "j"))), (PCons "Pair" ([PCons "NumDouble" ([PVar "i"]), PCons "NumInt" ([PVar "j"])]), EApp (EVar "NumDouble") (EApp (EApp (EVar "dbAdd") (EVar "i")) (EApp (EVar "intToDouble") (EVar "j")))), (PCons "Pair" ([PCons "NumInt" ([PVar "i"]), PCons "NumDouble" ([PVar "j"])]), EApp (EVar "NumDouble") (EApp (EApp (EVar "dbAdd") (EApp (EVar "intToDouble") (EVar "i"))) (EVar "j"))), (PCons "Pair" ([PCons "NumDouble" ([PVar "i"]), PCons "NumDouble" ([PVar "j"])]), EApp (EVar "NumDouble") (EApp (EApp (EVar "dbAdd") (EVar "i")) (EVar "j")))]))) Nothing,
    SLetRec "_mul" (ELambda ("x") (ELambda ("y") (ECase (EApp (EApp (EVar "Pair") (EVar "x")) (EVar "y")) [(PCons "Pair" ([PCons "NumInt" ([PVar "i"]), PCons "NumInt" ([PVar "j"])]), EApp (EVar "NumInt") (EApp (EApp (EVar "intMul") (EVar "i")) (EVar "j"))), (PCons "Pair" ([PCons "NumDouble" ([PVar "i"]), PCons "NumInt" ([PVar "j"])]), EApp (EVar "NumDouble") (EApp (EApp (EVar "dbMul") (EVar "i")) (EApp (EVar "intToDouble") (EVar "j")))), (PCons "Pair" ([PCons "NumInt" ([PVar "i"]), PCons "NumDouble" ([PVar "j"])]), EApp (EVar "NumDouble") (EApp (EApp (EVar "dbMul") (EApp (EVar "intToDouble") (EVar "i"))) (EVar "j"))), (PCons "Pair" ([PCons "NumDouble" ([PVar "i"]), PCons "NumDouble" ([PVar "j"])]), EApp (EVar "NumDouble") (EApp (EApp (EVar "dbMul") (EVar "i")) (EVar "j")))]))) Nothing,
    SLetRec "+" (EVar "_add") Nothing,
    -- SLetRec "×" (EVar "_mul") Nothing,

    -- pairs (idk if there was syntax planned around these?)

    STypeDef (TypeCons "Pair" ["a","b"] [DataCons "Pair" [TVar "a", TVar "b"]]),

    SLetRec "fst" (ELambda ("t") (ECase (EVar "t") [(PCons "Pair" ([PVar "x", PAny]), EVar "x")])) Nothing,

    SLetRec "snd" (ELambda ("t") (ECase (EVar "t") [(PCons "Pair" ([PAny, PVar "y"]), EVar "y")])) Nothing

    ]


hokiProg :: IO LProg
hokiProg = do
    errOrProg <- loadProgram hokiPrimProg
    case errOrProg of
        (Left err) -> putStrLn ("MAJOR ERROR: HOKI PRIMITIVES ARE BROKEN: " ++ err) >> return undefined
        (Right lprog) -> return lprog