module Functions where

import Subleq

import Data.List (intercalate)

-- end function ---------------------------------------------------------------
endF :: [Cell] -> [Cell]
endF [] = [Cell Nothing (Direct (-1))]
endF cs = cs

-- null function --------------------------------------------------------------
nullF :: [Cell] -> [Cell]
nullF [c] = [c, c]
nullF cs = cs

-- out function ---------------------------------------------------------------
outF :: [Cell] -> [Cell]
outF [c] = [c, Cell Nothing (Direct (-1))]
outF cs = cs

-- add function ---------------------------------------------------------------
addF :: [Cell] -> [Cell]
addF [s1, s2, tmp] = [s1, tmp, Cell Nothing (Offset 1), tmp, s2]
addF cs = cs

-- loop function --------------------------------------------------------------
loopF :: [Cell] -> [Cell]
loopF [p, to, inc, tmp1, tmp2] =
  intercalate [nxt]
    [ [inc, p]
    , nullF [tmp1]
    , addF [to, tmp1, tmp2]
    , nullF [tmp2]
    , [p, tmp1]]
  where
    nxt = Cell Nothing (Offset 1)
loopF cs = cs
