module LambdaStr where
    
    import LambdaExpr
    import LambdaArr hiding (map)
    import LambdaNum

    import Data.Char (ord)

    string :: String -> Expr
    string x = arr $ map (convert . ord) x
