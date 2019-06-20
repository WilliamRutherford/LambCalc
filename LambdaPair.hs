module LambdaPair where

    import Prelude hiding (fst, snd)

    import LambdaExpr
    import LambdaCalc hiding (true,  false, nil, null)
    import LambdaBool

    --pair (l x y f -> f x y)
    pair :: Expr -> Expr -> Expr
    pair x y = (Lambda "f" (App (App (Var "f") x ) y))

    --first (l p  -> p TRUE)
    fst :: Expr -> Expr
    fst p = apply p (true)

    --second (l p -> p FALSE)
    snd :: Expr -> Expr
    snd p = apply p (false)

