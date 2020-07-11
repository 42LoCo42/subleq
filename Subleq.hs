module Subleq where

import Data.Int (Int64)
import Data.Vector (Vector)

type Label = String

type Program = [Cell]

data Cell
  = Cell (Maybe Label) Code
  deriving (Show)

data Code
  = Direct Int64
  | Offset Int64
  | Labeled Label
  | LblOfst Label Int64
  deriving (Show)

data AssembledProgram =
  AP Int (Vector Int64)
  deriving (Show)
