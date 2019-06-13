-- | Library of parser definition and operations.
module ParserLib where

import Control.Applicative
import Data.Char
import Data.Functor
import Data.List

newtype Parser a = PsrOf{
    -- | Function from input string to:
    --
    --   * Nothing, if failure (syntax error);
    --   * Just (unconsumed input, answer), if success.
    dePsr :: String -> Maybe (String, a)}

-- Monadic Parsing in Haskell uses [] instead of Maybe to support ambiguous
-- grammars and multiple answers.

-- | Use a parser on an input string.
runParser :: Parser a -> String -> Maybe a
runParser (PsrOf p) inp = case p inp of
                            Nothing -> Nothing
                            Just (_, a) -> Just a
                          -- OR: fmap (\(_,a) -> a) (p inp)

-- | Read a character and return. Failure if input is empty.
anyChar :: Parser Char
anyChar = PsrOf p
  where
    p "" = Nothing
    p (c:cs) = Just (cs, c)

-- | Read a character and check against the given character.
char :: Char -> Parser Char
-- char wanted = PsrOf p
--   where
--     p (c:cs) | c == wanted = Just (cs, c)
--     p _ = Nothing
char wanted = satisfy (\c -> c == wanted)   -- (== wanted)

-- | Read a character and check against the given predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = PsrOf p
  where
    p (c:cs) | pred c = Just (cs, c)
    p _ = Nothing

-- | Expect the input to be empty.
eof :: Parser ()
eof = PsrOf p
  where
    p "" = Just ("", ())
    p _ = Nothing


-- | Read and check against a given string.
string :: String -> Parser String
string wanted = PsrOf p
  where
    p inp = case stripPrefix wanted inp of
              Nothing -> Nothing
              Just suffix -> Just (suffix, wanted)
            -- Refactor this!

-- But you have to compose smaller parsers to build larger parsers and to return
-- more interesting answers, e.g., abstract syntax trees.
--
-- This is what fmap, pure, <*>, >>= are for.  And there are more...

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (PsrOf p) = PsrOf q
                          -- (\inp -> fmap (\(rest, a) -> (rest, f a)) (p inp))
      where
        q inp = case p inp of
                  Nothing -> Nothing
                  Just (rest, a) -> Just (rest, f a)
                -- fmap (\(rest, a) -> (rest, f a)) (p inp)

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure a = PsrOf (\inp -> Just (inp, a))
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    -- Consider the 1st parser to be stage 1, 2nd parser stage 2.
    PsrOf p1 <*> PsrOf p2 = PsrOf q
      where
        q inp = case p1 inp of
                  Nothing -> Nothing
                  Just (middle, f) ->
                      case p2 middle of
                        Nothing -> Nothing
                        Just (rest, a) -> Just (rest, f a)
                      -- dePsr (fmap f (PsrOf p2)) middle

instance Alternative Parser where
    -- empty :: Parser a
    -- Always fail.  The identity for <|> below.
    empty = PsrOf (\_ -> Nothing)
    -- (<|>) :: Parser a -> Parser a -> Parser a
    -- Try the 1st one. If success, done; if failure, do the 2nd one
    PsrOf p1 <|> PsrOf p2 = PsrOf q
      where
        q inp = case p1 inp of
                  j@(Just _) -> j
                  Nothing -> p2 inp
    -- many :: Parser a -> Parser [a]
    -- 0 or more times, maximum munch, collect the answers into a list.
    -- Can use default implementation.

    -- some :: Parser a -> Parser [a]
    -- 1 or more times, maximum munch, collect the answers into a list.
    -- Can use default implementation.

instance Monad Parser where
    return = pure
    PsrOf p1 >>= k = PsrOf q
      where
        q inp = case p1 inp of
                  Nothing -> Nothing
                  Just (rest, a) -> dePsr (k a) rest

-- | Space or newline or tab.
whitespace :: Parser Char
whitespace = satisfy (\c -> c `elem` ['\t', '\n', ' '])

-- | Consume zero or more whitespaces, maximum munch.
whitespaces :: Parser String
whitespaces = many whitespace

-- | Read and check a terminal string, then skip trailing spaces.
terminal :: String -> Parser String
terminal wanted = string wanted <* whitespaces

-- | Read an integer, then skip trailing spaces.
integer :: Parser Integer
integer = sign <*> (read <$> some (satisfy isDigit)) <* whitespaces
  where
    sign = (char '-' *> pure negate) <|> pure id

-- | Read an identifier, then skip trailing spaces.  Disallow certain keywords.
identifier :: [String] -> Parser String
identifier keywords = do
    c <- satisfy isAlpha
    cs <- many (satisfy isAlphaNum)
    whitespaces
    let str = c:cs
    if str `elem` keywords then empty else return str

-- | One or more operands separated by an operator. Apply the operator(s) in a
-- left-associative way.
chainl1 :: Parser a               -- ^ operand parser
        -> Parser (a -> a -> a)   -- ^ operator parser
        -> Parser a               -- ^ evaluated answer
chainl1 arg op = do
    a <- arg
    more a
  where
    more x = do
        f <- op
        y <- arg
        more (f x y)
      <|>
        return x

-- | One or more operands separated by an operator. Apply the operator(s) in a
-- right-associative way.
chainr1 :: Parser a               -- ^ operand parser
        -> Parser (a -> a -> a)   -- ^ operator parser
        -> Parser a               -- ^ evaluated answer
chainr1 arg op = do
    x <- arg
    ((\f y -> f x y) <$> op <*> chainr1 arg op) <|> return x

-- | Parse a thing that is wrapped between open and close brackets.
between :: Parser open          -- ^ open bracket parser
        -> Parser close         -- ^ close bracket parser
        -> Parser a             -- ^ thing parser
        -> Parser a             -- ^ return the thing parsed
between open close p = open *> p <* close
