module LambdaArr where

    import Prelude hiding (fst, snd, map)
    import LambdaCalc hiding (nil, null)
    import LambdaNum
    import LambdaExpr
    import LambdaPair

    nil  :: Expr
    null :: Expr

    nil =  Lambda "__n" (true)
    null = Lambda "__p" (App (Var "__p") (Lambda "__xn" (Lambda "__yn" false)))

    arr :: [Expr] -> Expr
    arr []     = nil
    arr (x:xs) = pair x (arr xs)
    
    elm :: Expr -> Int -> Expr
    elm a b = if b == 0 then (fst a) else elm (snd a) (b-1)
    
    len :: Expr -> Int
    len x
        | x == nil  = 0
        | otherwise = 1 + len (snd x)

    --   l array-> func -> result
    map :: Expr -> Expr -> Expr
    map f x  
        | x == nil  = nil
        | otherwise = pair (applyh f (fst x)) (map f (snd x))  
