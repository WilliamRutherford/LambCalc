module LambdaParse where
    
    import Prelude hiding (null)

    import LambdaExpr
    import LambdaNum (convert)
    import ParserLib
    import LambdaPair
    import LambdaArr hiding (map)

    import Control.Applicative
    import Data.Char
    import Data.Functor
    import Data.List hiding (nil, null)
    import Data.Map

    {- 
        the environment of the interpreted program is stored here. Each "Name" refers to a variable, and each Maybe Expr represents it's value, or lack thereof.

        Given the pair ("x", Nothing) we know that x is a bounded variable. 
        This means that x was defined by a lambda function, and thus cannot be changed.

        Given the pair ("f", Just F) we know that the function f is defined as the expression F. 
        Keep in mind that this definition of F cannot contain f, as this would be self-referential.
        We cannot have self-referential functions, because a function being defined is not yet apart of
        our environment.
    -}
    type Env = Map Name (Maybe Expr)

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
    expr = int <|> var <|> lpair <|> larr <|> emptyArr <|> bexprs <|> lambda <* whitespaces

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

    lpair :: Parser Expr
    lpair = do
        char '('
        whitespaces
        x <- expr
        char ','
        whitespaces
        y <- expr
        char ')'
        whitespaces
        return (pair x y)

    emptyArr :: Parser Expr
    emptyArr = do
        a <- string "[]" 
        b <- whitespaces
        return nil
    
    larr :: Parser Expr
    larr = do
        char '['
        whitespaces
        x  <- expr
        xs <- many (char ',' *> whitespaces *> expr)
        char ']'
        return (arr (x:xs))
    
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

    funcn :: Parser Name
    funcn = do
        x  <- satisfy (not . isNumber)
        xs <- many anyChar
        return (x:xs)

    int :: Parser Expr
    int = convert <$> read <$> some (satisfy isDigit) <* whitespaces

    lambda ::  Parser Expr
    lambda = do
        string "("
        lambdaSym
        whitespaces
        x <- vars
        lambdaSplit
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

    lambdaSplit :: Parser String
    lambdaSplit = string "->" <|> string "."

    define :: Parser (Name, Expr)
    define = do
        x <- funcn
        string ":="
        y <- expr
        char "\n"
        return (x, y)

    spaces :: Parser String
    spaces = many (char " " <|> char "\t")
