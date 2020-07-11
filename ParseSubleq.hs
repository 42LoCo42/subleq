module ParseSubleq where

import Parser
import Subleq
import IntTools

import Data.Char (isAscii, isLetter, isDigit, ord)
import Data.Int (Int64)
import Data.List (intercalate)
import Control.Applicative ((<|>), many)

-- Helper ---------------------------------------------------------------------
singleton :: a -> [a]
singleton a = [a]

-- Inline space -----------------------------------------------------------------
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

lblDefP :: Parser (Maybe Label)
lblDefP = maybeP $ lblP <* charP ':'

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
cellP = Cell <$> lblDefP <*> codeP

-- String ---------------------------------------------------------------------
stringLiteralP :: Parser String
stringLiteralP =
  (\s -> read ("\"" ++ s ++ "\"")) <$>
  (charP '"' *> spanP (/= '"') <* charP '"')

normalSLP :: Parser [Cell]
normalSLP = map (Cell Nothing . Direct . ec . ord) <$> stringLiteralP

labeledSLP :: Parser [Cell]
labeledSLP = do
  lbl <- lblDefP
  sl <- normalSLP
  let (Cell _ c : t) = sl
  return $ if null sl then [] else (Cell lbl c : t)

-- Generic function parser ----------------------------------------------------
functionP :: String -> ([Cell] -> [Cell]) -> Parser [Cell]
functionP name f = f <$> (
  stringP name *> charP '(' *> (lineP <|> pure []) <* charP ')')

-- null function --------------------------------------------------------------
nullF :: [Cell] -> [Cell]
nullF [c@(Cell Nothing _)] = [c, c]
nullF cs = cs

-- add function ---------------------------------------------------------------
addF :: [Cell] -> [Cell]
addF [s1@(Cell Nothing _), s2@(Cell Nothing _), tmp@(Cell Nothing _)] =
  [s1, tmp, Cell Nothing (Offset 1), tmp, s2]
addF cs = cs

-- loop function --------------------------------------------------------------
loopF :: [Cell] -> [Cell]
loopF
  (p@(Cell Nothing _) : to@(Cell Nothing _) :
  inc@(Cell Nothing _) :
  tmp1@(Cell Nothing _) : tmp2@(Cell Nothing _) : f) =
  f ++ intercalate [nxt]
    [ [inc, p]
    , nullF [tmp1]
    , addF [to, tmp1, tmp2]
    , nullF [tmp2]
    , [p, tmp1]]
  where
    nxt = Cell Nothing (Offset 1)
loopF cs = cs

-- Function group -------------------------------------------------------------
allFunctionsP :: Parser [Cell]
allFunctionsP =
  functionP "null" nullF <|> functionP "add" addF <|>
  functionP "loop" loopF

-- Cells group ----------------------------------------------------------------
cellsP :: Parser [Cell]
cellsP = allFunctionsP <|> singleton <$> cellP <|> labeledSLP <|> normalSLP

-- Line group -----------------------------------------------------------------
lineP :: Parser [Cell]
lineP = const [] <$> commentP <|> concat <$> sepBy wsP cellsP

-- Program group --------------------------------------------------------------
programP :: Parser Program
programP = concat <$> many (lineP <* charP '\n')
