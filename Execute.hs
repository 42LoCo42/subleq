module Execute where

import Subleq
import IntTools

import Data.Int (Int64)
import Data.Vector ((!), (//))
import Data.Char (chr, ord)
import qualified Data.Vector as V

execute :: Int64 -> AssembledProgram -> IO ()
execute ix (AP bits ap) =
  if ix < 0
  then putStrLn "Exited, jump to negative address."
  else if ix + 3 > ec (V.length ap)
  then putStrLn "Exited, not enough data left."
  else do
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
    else do
      if tgtA < 0
      then do
        putChar $ chr $ ec val
        execute jmpA nr
      else do
        if result > 0
        then execute (ix + 3) nr
        else execute jmpA nr
