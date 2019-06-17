module LambdaCalc where
    
    import LambdaParse   
    import LambdaExpr
    import Data.List
    --import Data.Hash

    --constants types
    true :: Expr
    false :: Expr
    id :: Expr

    --constants

    true  = Lambda "_xt" (Lambda "_yt" (Var "_xt"))
    false = Lambda "_xf" (Lambda "_yf" (Var "_yf"))

    n0 = Lambda "f" (Lambda "x" (Var "x"))
    n1 = Lambda "f" (Lambda "x" (App (Var "f") (Var "x")))

    id = Lambda "__x__" (Var "__x__")

    instance Show Expr where
        show x
           | x == true  = "true"
           | x == false = "false"
           | otherwise  = showExpr x


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

    treeView :: Expr -> String
    treeView (Var a) = "(V " ++ a ++ ")"
    treeView (App x y) = "(A " ++ treeView x ++ " " ++ treeView y ++ ")"
    treeView (Lambda a x) = "(L " ++ a ++ " - " ++ treeView x ++ ")"

    --makes sure that lambda variables don't overlap
    apply :: Expr -> Expr -> Expr
    apply x y = applyh x (if overlapped == [] then y else foldr (\a b -> sub b a (Var ("_"++a))) y overlapped)
        where overlapped = overlap x y
    --x, y :: Expr
    --(\a b ->) :: a -> b -> b (b is Expr)

    mapply :: Expr -> [Expr] -> Expr
    mapply x []     = x
    mapply x (y:[]) = applyh x y
    mapply x (y:ys) = mapply (applyh x y) ys 

    applyh :: Expr -> Expr -> Expr
    applyh (Var a) y      = App (Var a) y
    applyh (Lambda a x) y = sub x a y  
    applyh x y = App x y

    sub :: Expr -> Name -> Expr -> Expr
    sub (Var a)      q r = if a == q then r else Var a
    sub (Lambda a x) q r = if a /= q then Lambda a (sub x q r) else (Lambda a x)--else error "trying to replace lambda var"
    sub (App x y)    q r = apply (sub x q r) (sub y q r) 

    overlap :: Expr -> Expr -> [Name]
    overlap x y = filter (\a -> a `elem`listVars y) (listVars x)

    listVars :: Expr -> [Name]
    listVars (Var a) = [a]
    listVars (App x y) = nub $ listVars x ++ listVars y
    listVars (Lambda a x) = nub $ a:(listVars x)
