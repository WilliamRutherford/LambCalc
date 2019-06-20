module LambdaArr where

    import LambdaCalc hiding (nil, null)
    import LambdaExpr
    import LambdaPair

    nil  :: Expr
    null :: Expr

    nil =  Lambda "__n" (true)
    null = Lambda "__p" (App (Var "__p") (Lambda "__xn" (Lambda "__yn" false)))

    arr :: [Expr] -> Expr
    arr []     = nil
    arr (x:xs) = pair x (arr xs)
