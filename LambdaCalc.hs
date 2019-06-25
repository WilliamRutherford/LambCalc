module LambdaCalc where
    
    --import LambdaParse   
    import LambdaExpr
    import Data.List hiding (nil, null)
    --import Data.Hash

    --constants types
    true :: Expr
    false :: Expr
    id :: Expr

    --constants

    true  = Lambda "_xt" (Lambda "_yt" (Var "_xt"))
    false = Lambda "_xf" (Lambda "_yf" (Var "_yf"))

    nil = Lambda "__n" (true) 

    n0 = Lambda "f" (Lambda "x" (Var "x"))
    n1 = Lambda "f" (Lambda "x" (App (Var "f") (Var "x")))

    id = Lambda "__x__" (Var "__x__")

    treeView :: Expr -> String
    treeView (Var a) = "(V " ++ a ++ ")"
    treeView (App x y) = "(A " ++ treeView x ++ " " ++ treeView y ++ ")"
    treeView (Lambda a x) = "(L " ++ a ++ " - " ++ treeView x ++ ")"

    --makes sure that lambda variables don't overlap
    apply :: Expr -> Expr -> Expr
    apply x y = applyh x (if overlapped == [] then y else foldr (\a b -> subName b a (a++"_")) y overlapped)
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
    sub (App x y)    q r = applyh (sub x q r) (sub y q r) 

    subName :: Expr -> Name -> Name -> Expr
    subName (Var a)      q r = Var (if a == q then r else a)
    subName (Lambda a x) q r = Lambda (if a == q then r else a) (subName x q r)
    subName (App x y)    q r = App (subName x q r) (subName y q r)

    overlap :: Expr -> Expr -> [Name]
    overlap x y = filter (\a -> a `elem`listVars y) (listVars x)

    listVars :: Expr -> [Name]
    listVars (Var a) = [a]
    listVars (App x y) = nub $ listVars x ++ listVars y
    listVars (Lambda a x) = nub $ a:(listVars x)
