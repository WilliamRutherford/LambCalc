module LambdaArr where

    import Prelude hiding (fst, snd, map)
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
    
    --   l array-> func -> result
    map :: Expr -> Expr -> Expr
    map f x  
        | x == nil  = nil
        | otherwise = pair (applyh f (fst x)) (map f (snd x))  
