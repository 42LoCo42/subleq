module Execute where

import IntTools
import Subleq

import Data.Char (chr, ord)
import Data.Int (Int64)
import Data.Vector ((!), (//))
import qualified Data.Vector as V

execute :: Int64 -> AssembledProgram -> IO ()
execute ix (AP bits ap)
  | ix < 0
  = putStrLn "Exited, jump to negative address."
  | ix + 3 > ec (V.length ap)
  = putStrLn "Exited, not enough data left."
  | otherwise
  = do
    let
      valA = ap ! ec ix
      tgtA = ap ! ec (ix + 1)
      jmpA = ap ! ec (ix + 2)
      val  = ap ! ec valA
      tgt  = ap ! ec tgtA
      result = tgt - val
      ap' newVal = if tgtA < 0 then ap else ap // [(ec tgtA, newVal)]
      next newVal = AP bits $ ap' newVal
      nr = next result
    if valA < 0
    then do
      ch <- ec . ord <$> getChar :: IO Int64
      execute jmpA (next ch)
    else
      if tgtA < 0
      then do
        putChar $ chr $ ec val
        execute jmpA nr
      else
        if result > 0
        then execute (ix + 3) nr
        else execute jmpA nr
