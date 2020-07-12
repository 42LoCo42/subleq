module ParseSubleq where

import Functions
import IntTools
import Parser
import Subleq

import Control.Applicative ((<|>), many)
import Data.Char (isAscii, isDigit, isLetter, isSpace, ord)
import Data.Int (Int64)

-- Helper ---------------------------------------------------------------------
singleton :: a -> [a]
singleton a = [a]

-- Inline space ---------------------------------------------------------------
wsP :: Parser String
wsP = spanP (`elem` [' ', '\t'])

-- Comment --------------------------------------------------------------------
commentP :: Parser String
commentP = charP '#' *> spanP (/= '\n')

-- Label ----------------------------------------------------------------------
maybeP :: Parser a -> Parser (Maybe a)
maybeP (Parser p1) = Parser $ \input ->
  maybe (Just (input, Nothing)) (\(s, a) -> Just (s, Just a)) (p1 input)

lblP :: Parser Label
lblP = spanP (\c -> isAscii c && (isLetter c || isDigit c))

lblDefP :: Parser Label
lblDefP = lblP <* (ms <* charP ':' <* ms)
  where
    ms = maybeP $ spanP isSpace

-- Numbers --------------------------------------------------------------------
numP :: Parser Int64
numP = read <$> spanP (\c -> isAscii c && isDigit c)

signedNumP :: Parser Int64
signedNumP = do
  sign <- charP '+' <|> charP '-'
  num <- show <$> numP
  return $ read $ if sign == '+' then num else sign : num

-- Offset ---------------------------------------------------------------------
offsetP :: Parser Int64
offsetP = charP '?' *> (signedNumP <|> pure 0)

-- Character ------------------------------------------------------------------
asciiP :: Parser Int64
asciiP = toEnum . ord <$>
  (charP '\'' *> cpredP (/= '\'') <* charP '\'')

-- Labeled offset -------------------------------------------------------------
lblOfstP :: Parser Code
lblOfstP = LblOfst <$> lblP <*> signedNumP

-- Code group -----------------------------------------------------------------
codeP :: Parser Code
codeP =
  lblOfstP <|>
  Direct <$> (signedNumP <|> numP <|> asciiP) <|>
  Offset <$> offsetP <|>
  Labeled <$> lblP

-- Cell group -----------------------------------------------------------------
cellP :: Parser Cell
cellP =
  Cell <$> maybeP lblDefP <*> codeP <|>
  Cell Nothing (Offset 1) <$ charP ';'

-- String ---------------------------------------------------------------------
stringLiteralP :: Parser [Cell]
stringLiteralP =
  map (Cell Nothing . Direct . ec . ord) .
  (\s -> read ("\"" ++ s ++ "\"")) <$>
  (charP '"' *> spanP (/= '"') <* charP '"')

-- Labeled cells --------------------------------------------------------------
lbldCellsP :: Parser [Cell] -> Parser [Cell]
lbldCellsP p = do
  lbl <- lblDefP
  cs <- p
  let (Cell _ c : t) = cs
  return $ if null cs then [] else Cell (Just lbl) c : t

-- Generic function parser ----------------------------------------------------
functionP :: String -> ([Cell] -> [Cell]) -> Parser [Cell]
functionP name f = f <$> (
  stringP name *> charP '(' *> (lineP <|> pure []) <* charP ')')

-- Function group -------------------------------------------------------------
allFunctionsP :: Parser [Cell]
allFunctionsP =
  foldr1 (<|>) $ map (uncurry functionP)
  [ ("end" , endF )
  , ("null", nullF)
  , ("out" , outF )
  , ("add" , addF )
  , ("loop", loopF)
  ]

-- Cells group ----------------------------------------------------------------
cellsP :: Parser [Cell]
cellsP = foldr1 (<|>) (lbldGroups ++ realGroups) <|> singleton <$> cellP
  where
    realGroups = [allFunctionsP, stringLiteralP]
    lbldGroups = map lbldCellsP realGroups

-- Line group -----------------------------------------------------------------
lineP :: Parser [Cell]
lineP = [] <$ commentP <|> concat <$> sepBy wsP cellsP

-- Program group --------------------------------------------------------------
programP :: Parser Program
programP = concat <$> many (lineP <* charP '\n')
