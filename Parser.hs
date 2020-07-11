module Parser where

import Control.Applicative

newtype Parser a = Parser {
  runParser :: String -> Maybe (String, a)
}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', x) <- p input
    Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f)  <- p1 input
    (input'', x) <- p2 input'
    Just (input'', f x)

instance Monad Parser where
  (Parser p1) >>= f = Parser $ \input -> do
    (input', a) <- p1 input
    let (Parser p2) = f a
    p2 input'

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    p1 input <|> p2 input

-- Constructs a parser for a single char
charP :: Char -> Parser Char
charP c = Parser f where
  f []          = Nothing
  f (x:xs)
    | c == x    = Just (xs, c)
    | otherwise = Nothing

-- Uses chaining to parse a string
stringP :: String -> Parser String
stringP = traverse charP

-- Parses a char based on a predicate
cpredP :: (Char -> Bool) -> Parser Char
cpredP f = Parser $ \input@(h:t) ->
  if null input then Nothing
  else if f h then Just (t, h)
  else Nothing

-- Parses a range of consecutive elements based on a predicate
spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> do
  let (token, rest) = span f input
  if null token
    then Nothing
    else Just (rest, token)

-- Creates a parser that fails if the original one returns nothing
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs
    then Nothing
    else Just (input', xs)

-- A Parser to separate elements from eachother
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep elemP = (:) <$> elemP <*> many (sep *> elemP) <|> pure []

-- A Parser that parses multiple or no elements
multOrNone :: Parser a -> Parser [a]
multOrNone p = many p <|> pure []

optional :: a -> Parser a -> Parser a
optional a p = p <|> pure a
