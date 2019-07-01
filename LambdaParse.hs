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

    type Env = Map (Name) (Maybe Expr)

    parse :: String -> Expr
    parse = fromJust . parseM
        where
        fromJust Nothing  = error "Error Parsing Term"
        fromJust (Just a) = a

    parseFile :: String -> Env
    parseFile = fromJust . parseFileM
        where
        fromJust Nothing  = error "Error reading file using parseFile"
        fromJust (Just a) = a

    readLambdaFile :: String -> IO Env
    readLambdaFile filename = parseFile <$> readFile filename

    parseM :: String -> Maybe Expr
    parseM = runParser termParser

    parseFileM :: String -> Maybe Env
    parseFileM = runParser fileParser

    fileParser :: Parser Env
    fileParser = do
        whitespaces
        x <- some define
        eof
        return $ Data.Map.fromList x


    termParser :: Parser Expr
    termParser = do
        spaces
        t <- exprs
        eof
        return t

    expr :: Parser Expr
    expr = int <|> var <|> lpair <|> larr <|> emptyArr <|> bexprs <|> lambda <* spaces

    bexprs :: Parser Expr
    bexprs = do
        char '('
        spaces
        x <- some expr
        char ')'
        spaces
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
        spaces
        x <- expr
        char ','
        spaces
        y <- expr
        char ')'
        spaces
        return (pair x y)

    emptyArr :: Parser Expr
    emptyArr = do
        string "[]" 
        spaces
        return nil
    
    larr :: Parser Expr
    larr = do
        char '['
        spaces
        x  <- expr
        xs <- many (char ',' *> spaces *> expr)
        char ']'
        spaces
        return (arr (x:xs))
    
    vars :: Parser [Name]
    vars = some varn

    var :: Parser Expr
    var = fmap Var varn

    varn :: Parser Name
    varn = do
        x  <- satisfy (\x -> not $ isNumber x || x `elem` ['\n',' ','\t'])
        xs <- many varChar
        spaces
        return (x:xs)

    varChar :: Parser Char
    varChar = satisfy (\x -> not $ x `elem` ['\n', ' ', '\t'])

    int :: Parser Expr
    int = convert <$> read <$> some (satisfy isDigit) <* spaces

    lambda ::  Parser Expr
    lambda = do
        string "("
        lambdaSym
        spaces
        x <- vars
        lambdaSplit
        spaces
        y <- exprs
        string ")"
        spaces
        return (expandLambda x y)

    expandLambda :: [Name] -> Expr -> Expr
    expandLambda (x:[]) y = (Lambda x y)
    expandLambda (x:xs) y = (Lambda x (expandLambda xs y))

    lambdaSym :: Parser String
    lambdaSym = string "l" <|> string "\\"

    lambdaSplit :: Parser String
    lambdaSplit = string "->" <|> string "."

    define :: Parser (Name,Maybe Expr)
    define = do
        x <- varn
        string ":="
        spaces
        y <- expr
        whitespaces
        return (x, Just y)

    spaces :: Parser String
    spaces = many (satisfy (\x -> x `elem` [' ', '\t']))
