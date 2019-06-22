module LambdaComb where

    import LambdaExpr
    import LambdaParse

    i, k, s, b, c, w, u, omega, y :: Expr

  --I := (lx.x)
    i = parse "(lx.x)"

  --K := (lx y. x) equiv to true
    k = parse "(lx y. x)"

  --S := (lx y z. x z (y z))
    s = parse "(lx y z. x z (y z))"

  --B := (lx y z. x (y z))
    b = parse "(lx y z. x (y z))"

  --C := (lx y z. x z y)
    c = parse "(lx y z. x z y)"

  --W := (lx y. x y y)
    w = parse "(lx y. x y y)"

  --U := (lx . x x)
    u = parse "(lx.x x)"

--Omega := U U
    omega = App u u

--  Y := (lg. (lx. g (x x)) (lx. g (x x)))
    y = parse "(lg. (lx. (g) (x x)) (ly. (g) (y y)))"
{-  
    yf :: Expr
    yf = (Lambda "__gy" (App (Lambda "__xy" (App (Var "__gy") (App (Var "__xy") (Var "__xy")))) (Lambda "__xy" (App (Var "__gy") (App (Var "__xy") (Var "__xy"))))))
-}
