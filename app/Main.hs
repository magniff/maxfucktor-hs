module Main where

import Data.List (intercalate)
import Maxfucktor.Generator (renderProgram)
import Maxfucktor.Optimizer (optimize)
import Maxfucktor.Parser (Parser (runParser), wholeProgram)
import System.Environment (getArgs)


programHeader :: [[Char]]
programHeader = [
  "global _start",
  "section .data",
  "  memory: times 32768 db 0",
  "section .text",
  "_start:",
  "  mov rsi, memory",
  "  mov rdx, 1 ;; rdx wont change during the runtime",
  "  mov rdi, 1 ;; rdi represents an io descriptor, typically 1 or 0",
  "  jmp run",
  "exit:",
  "  mov rax, 60",
  "  mov rdi, 0",
  "  syscall",
  "run:\n"
  ]


main :: IO ()
main =
  do
    args <- getArgs
    datum <- readFile $ head args
    let filtered = filter (`elem` "<>.,[]+-") datum in
      case runParser wholeProgram filtered of
        (Nothing, leftover) ->
          print $ "NoParser error occured, unable to process: " ++ leftover
        (Just ast, _) ->
          let opt_ast = map optimize ast
            in putStr $
            intercalate "\n" programHeader ++
            (intercalate "\n" (renderProgram opt_ast) ++ "\n")
