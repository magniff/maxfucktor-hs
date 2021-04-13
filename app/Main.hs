{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}


module Main where


import System.Environment ( getArgs )
import Maxfucktor.Parser ( wholeProgram, Parser(runParser) )


main :: IO ()
main =
  do
    args <- getArgs
    datum <- readFile $ head args
    let filtered = filter (`elem` "<>.,[]+-") datum in
      print $ runParser wholeProgram filtered
