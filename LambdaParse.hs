module LambdaParse where
    
    import LambdaCalc

    parse :: String -> Expr
    parse "" = error "empty input into parser"
