module LambdaShow where
    
    import Prelude hiding (fst, snd)
    import LambdaExpr
    import LambdaPair
    import LambdaArr
    import LambdaBool
    import LambdaNum

    instance Show Expr where
        show x
           | x == nil   = "[]"
           | isBool x   = printBool x
           | isNum  x   = show (toNum x)
           | isPair x   = printPair x
           | otherwise  = showExpr x

    {- booleans have the form (lx y. x) or (lx y. y) -}
    isBool :: Expr -> Bool
    isBool (Lambda a (Lambda b (Var c))) = (a == c) || (b == c)
    isBool _                             = False

    printBool :: Expr -> String
    printBool (Lambda a (Lambda b (Var c)))
        | c == a = "true"
        | c == b = "false"
        | otherwise = (error "tried to use printBool to print non-boolean function")


    {-  pairs have the form:
        (lf. f a b) 
    -}
    isPair :: Expr -> Bool
    isPair (Lambda a (App (App (Var b) _) _)) = (a == b)
    isPair _ = False

    printPair :: Expr -> String
    printPair (Lambda _ (App (App _ a) b)) = if isPair b then "[" ++ printArr a b ++ "]" else "("++show a ++", " ++ show b ++ ")"

    printArr :: Expr -> Expr -> String
    printArr a b = if b == nil then show a else show a ++ "," ++ printArr (fst b) (snd b)
        

    showExpr :: Expr -> String
    showExpr (Var x)   = x
    showExpr (App x y) = "("++ (showExpr x) ++ ") (" ++ (showExpr y) ++ ")"
    showExpr (x)       = "(l" ++ writeVars (showVars x) ++". " ++ (writeBody x)++ ")"

    showVars :: Expr -> [Name]
    showVars (Lambda x y) = (x:(showVars y))
    showVars (App (Lambda x y) _) = x:(showVars y)
    showVars (x) = []

    writeVars :: [Name] -> String
    writeVars []     = ""
    writeVars (x:[]) = x
    writeVars (x:xs) = x ++ " " ++ (writeVars xs)

    writeBody :: Expr -> String
    writeBody (Var x)      = x
    writeBody (App x y)    = showExpr x ++ " " ++ showExpr y
    writeBody (Lambda x y) = writeBody y


