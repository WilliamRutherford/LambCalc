module LambdaEq where

    import LambdaCalc
    import LambdaExpr
    
    import Data.Map as Map

    instance Eq Expr where
        (==) a b = equalContext a b (Map.empty)       

    equal :: Expr -> Expr -> Expr
    equal a b = if (equalContext a b (Map.empty)) then true else false

    {- An equals function that takes in the context of functions.

                    lamb -> lamb -> context        -> result -} 
    equalContext :: Expr -> Expr -> Map Name Name -> Bool
    equalContext (Var a) (Var b) c           = (c ! a) == b 
    equalContext (App a b) (App x y) c       = (equalContext a x c) && (equalContext b y c)
    equalContext (Lambda a x) (Lambda b y) c = equalContext x y (insert a b c)
