{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}


module Maxfucktor.Optimizer where


import qualified Maxfucktor.Parser as P


data AST = 
    Inc     Int
  | Dec     Int
  | GoLeft  Int
  | GoRight Int
  | Input   Int
  | Output  Int
  | Loop    [AST]
  | Drop
  | Add     Int
  | Sub     Int
  | Mul     {shift0::Int, shift1::Int, mul0::Int, mul1::Int}
  deriving (Show, Eq)


optimize :: P.AST -> AST
optimize astnode = 
    case astnode of
        P.Loop [P.Dec 1, P.GoRight s0, P.Inc 1, P.GoLeft s1] | s0 == s1 -> Add s0
        P.Loop [P.Dec 1, P.GoLeft s0, P.Inc 1, P.GoRight s1] | s0 == s1 -> Add (-s0)
        P.Loop [P.Dec 1, P.GoRight s0, P.Dec 1, P.GoLeft s1] | s0 == s1 -> Sub s0
        P.Loop [P.Dec 1, P.GoLeft s0, P.Dec 1, P.GoRight s1] | s0 == s1 -> Sub (-s0)
        P.Loop
            [P.Dec 1, P.GoRight p0, P.Inc p1, P.GoRight p2, P.Inc p3, P.GoLeft p4]
            | p0 + p2 == p4 ->
            Mul p0 (p0+p2) p1 p3
        P.Loop [P.Dec value] | value > 0 -> Drop
        P.Loop contents -> Loop $ map optimize contents
        P.Inc     value -> Inc value
        P.Dec     value -> Dec value
        P.GoLeft  value -> GoLeft value
        P.GoRight value -> GoRight value
        P.Input   value -> Input value
        P.Output  value -> Output value
