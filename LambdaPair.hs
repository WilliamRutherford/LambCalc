module LambdaPair where

    import Prelude hiding (fst, snd)

    import LambdaExpr
    import LambdaCalc hiding (true,  false, nil, null)
    import LambdaBool

    --pair (l x y f -> f x y)
    pair :: Expr -> Expr -> Expr
    pair x y = (Lambda "pair" (App (App (Var "pair") x ) y))

    --first (l p  -> p TRUE)
    fst :: Expr -> Expr
    fst p = applyh p (true)

    --second (l p -> p FALSE)
    snd :: Expr -> Expr
    snd p = applyh p (false)

