module LambdaParse where
    
    import LambdaExpr
    import ParserLib
    
    import Control.Applicative
    import Data.Char
    import Data.Functor
    import Data.List

    --currently, "(a (b c))" works but "((b c) d)" does not

    parse :: String -> Expr
    parse = fromJust . parseM
        where
        fromJust Nothing  = error "Error Parsing Term"
        fromJust (Just a) = a

    parseM :: String -> Maybe Expr
    parseM = runParser termParser

    termParser :: Parser Expr
    termParser = do
        whitespaces
        t <- exprs
        eof
        return t

    expr :: Parser Expr
    expr = var <|> bexprs <|> lambda <* whitespaces

    bexprs :: Parser Expr
    bexprs = do
        char '('
        whitespaces
        x <- some expr
        char ')'
        whitespaces
        return (expandExprs x)

    exprs :: Parser Expr
    exprs = do
        x <- some expr
        return (expandExprs x)

    expandExprs :: [Expr] -> Expr
    expandExprs (x:[])     = x
    expandExprs x = foldl1 (App) x

    vars :: Parser [Name]
    vars = some varn

--can var and varn be combined?

    varn :: Parser Name
    varn = do
        x <- satisfy isAlpha
        xs <- many (satisfy isAlphaNum)
        whitespaces
        return (x:xs)

    var :: Parser Expr
    var = do
        x  <-        satisfy isAlpha
        xs <- many  (satisfy isAlphaNum)
        whitespaces
        return (Var (x:xs))


    lambda ::  Parser Expr
    lambda = do
        string "("
        lambdaSym
        whitespaces
        x <- vars
        string "->"
        whitespaces
        y <- exprs
        string ")"
        whitespaces
        return (expandLambda x y)

    expandLambda :: [Name] -> Expr -> Expr
    expandLambda (x:[]) y = (Lambda x y)
    expandLambda (x:xs) y = (Lambda x (expandLambda xs y))

    lambdaSym :: Parser String
    lambdaSym = string "l" <|> string "\\"

