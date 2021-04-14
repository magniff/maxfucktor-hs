{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}


module Main where


import System.Environment ( getArgs )
import Maxfucktor.Parser ( wholeProgram, Parser(runParser) )
import Maxfucktor.Optimizer ( optimize )


main :: IO ()
main =
  do
    args <- getArgs
    datum <- readFile $ head args
    let filtered = filter (`elem` "<>.,[]+-") datum in
      case runParser wholeProgram filtered of
        (Nothing, leftover) -> print $ "NoParser error occured, unable to process: " ++ leftover
        (Just ast, _) -> print $ map optimize ast
