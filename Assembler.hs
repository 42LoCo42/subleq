module Assembler where

import IntTools
import Subleq

import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

assemble :: Int -> Program -> AssembledProgram
assemble bits program = AP bits $ V.fromList resolved
  where
    ixed = zip program [0 :: Int64 ..]
    ixedLbls =
      map (\(Cell (Just lbl) _, ix) -> (lbl, ix)) $
      filter hasLabel ixed
    resolved = map (changeBits 64 bits . toDirect ixedLbls) ixed

hasLabel :: (Cell, Int64) -> Bool
hasLabel (Cell (Just _) _, _) = True
hasLabel _ = False

resolveOffset :: Int64 -> Code -> Code
resolveOffset pos (Offset o) = Direct (o + pos)
resolveOffset _ c = c

resolveLabel :: [(Label, Int64)] -> Code -> Code
resolveLabel ixedLbls (Labeled lbl) = Direct ix
  where
    mIx = lookup lbl ixedLbls
    ix = fromMaybe 0 mIx
resolveLabel ixedLbls (LblOfst lbl o) = Direct (ix + o)
  where
    (Direct ix) = resolveLabel ixedLbls (Labeled lbl)
resolveLabel _ c = c

toDirect :: [(Label, Int64)] -> (Cell, Int64) -> Int64
toDirect ixedLbls (Cell _ code, ix) = d
  where
    Direct d = resolveOffset ix $ resolveLabel ixedLbls code
