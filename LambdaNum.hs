module LambdaNumber where

    import LambdaCalc hiding (n0, n1)
    import Prelude hiding (exp)

    convert :: Int -> Expr
    convert x = (Lambda "f" (Lambda "x" (convertHelp x)))

    convertHelp :: Int -> Expr
    convertHelp 0 = (Var "x")
    convertHelp x = App (Var "f") (convertHelp (x-1))

    toNum :: Expr -> Int
    toNum (Lambda _ (Lambda _ x)) = toNumHelper x

    toNumHelper :: Expr -> Int
    toNumHelper (Var _) = 0
    toNumHelper (Lambda _ x) = (toNumHelper x) - 1

    -- isZero: (\x -> x (\y -> false) true
    isZero :: Expr -> Expr
    isZero = apply (Lambda "_x0" (App (App (Var "_x0") (Lambda "_y0" false)) true))

    -- n : \f x -> f ... f x
    -- m : \f x -> f ... f x
    exp :: Expr -> Expr -> Expr
    exp n m = applyh n m
    
