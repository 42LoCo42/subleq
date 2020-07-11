module Main where

import Parser
import Subleq
import ParseSubleq
import Assembler
import Execute

import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  if null args
  then putStrLn "Specify file to execute!"
  else do
    raw <- readFile $ head args
    let (Just (_, program)) = runParser programP raw
    let ap@(AP _ p) = assemble 8 program
    print ap
    print $ length p
    execute 0 ap
