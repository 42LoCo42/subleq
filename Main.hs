module Main where

import Assembler
import Execute
import ParseSubleq
import Parser
import Subleq

import System.Environment (getArgs)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  if null args
  then putStrLn "Specify file to execute!"
  else do
    raw <- readFile $ head args
    let (Just (_, program)) = runParser programP raw
    let ap@(AP _ p) = assemble 64 program
    print ap
    print $ length p
    execute 0 ap
