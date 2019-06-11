module LambdaNumber where

    import LambdaCalc hiding (n0, n1)
    import Prelude hiding (exp, succ)


    n0 :: Expr -- \f x -> x
    n1 :: Expr -- \f x -> f x
    n2 :: Expr -- \f x -> f (f x)

    n0 = (Lambda "__f" (Lambda "__x"                                   (Var "__x")))   --     x
    n1 = (Lambda "__f" (Lambda "__x"                  (App (Var "__f") (Var "__x"))))  --   f x
    n2 = (Lambda "__f" (Lambda "__x" (App (Var "__f") (App (Var "__f") (Var "__x"))))) --f (f x)

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
    toNumHelper _ = error "not a number"

    -- isZero: (\x -> x (\y -> false) true
    isZero :: Expr -> Expr
    isZero = apply (Lambda "_x0" (App (App (Var "_x0") (Lambda "_y0" false)) true))

    -- SUCC := (\n f x -> f (n f x))
    succ :: Expr -> Expr -> Expr -> Expr
    succ n f x = applyh (applyh (applyh (Lambda "ns" (Lambda "fs" (Lambda "xs" (App (Var "fs") (App (App (Var "ns") (Var "fs")) (Var "xs")))))) x) f) n 

    -- n : \f x -> f ... f x
    -- m : \f x -> f ... f x
    -- POW := \n m -> m n
    exp :: Expr -> Expr -> Expr
    exp n m = apply (apply (Lambda "__n__" (Lambda "__m__" (App (Var "__m__") (Var "__n__")))) m) n
    
