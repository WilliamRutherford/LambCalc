module LambdaBool where


    import LambdaExpr
    import LambdaEq
    import LambdaCalc hiding (true, false)
    import Prelude hiding (not, or, and)

    true :: Expr
    false :: Expr

    not :: Expr -> Expr

    and :: Expr -> Expr -> Expr
    or  :: Expr -> Expr -> Expr
    xor :: Expr -> Expr -> Expr


    -- lx y. x
    true  = Lambda "_xt" (Lambda "_yt" (Var "_xt"))

    -- lx y . y
    false = Lambda "_xf" (Lambda "_yf" (Var "_yf"))

    -- lx . (x FALSE) TRUE
    -- not
    not = apply (Lambda "_" (App (App (Var "_") false) true)) 

    -- lp x y -> p (x) (y)
    -- if/then/else
    ifelse p x y = if (p == true) then x else y

    -- lx y. x x y
    -- or
    or x y =  apply (apply (Lambda "_x" (Lambda "_y" (App (App (Var "_x") (Var "_x")) (Var "_y")))) x) y

    -- lx y. x y x
    -- and
    and x y = apply (apply (Lambda "_x" (Lambda "_y" (App (App (Var "_x") (Var "_y")) (Var "_x")))) x) y


    --lx y. x (not y) y
    --xor
    xor x y = apply (apply (Lambda "_x" (Lambda "_y" (App (App (Var "_x") (not (Var "_y"))) (Var "_y")))) x) y

