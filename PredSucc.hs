module PredSucc where

    import Prelude hiding (fst, snd, succ)
    import LambdaCalc
    import LambdaExpr
    import LambdaParse
    import LambdaPair

    -- (\n f x -> f (n f x))
    succ :: Expr -> Expr -> Expr -> Expr
    succ n f x = apply (apply (apply (App (Var "fs") (App (App (Var "ns") (Var "fs")) (Var "xs"))) n) f) x

    succf :: Expr
    succf = (App (Var "fs") (App (App (Var "ns") (Var "fs")) (Var "xs")))

    phi :: Expr -> Expr
    phi = apply (Lambda "xp" (pair (snd (Var "xp")) (applyh succf (snd (Var "xp")))))


    -- λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)
    predf :: Expr
    predf = parse "(l n f x -> n (l g h -> h (g f)) (l u -> x) (l u -> u))"
