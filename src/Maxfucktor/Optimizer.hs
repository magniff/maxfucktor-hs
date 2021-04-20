{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Maxfucktor.Optimizer where


import Maxfucktor.AST
  ( AST
      ( Add,
        Dec,
        Drop,
        GoLeft,
        GoRight,
        Inc,
        Input,
        Loop,
        Mul,
        Output,
        Sub
      ),
    Optimized,
    UnOptimized,
  )


optimize :: AST UnOptimized -> AST Optimized
optimize astnode =
  case astnode of
    Loop [Dec 1, GoRight s0, Inc 1, GoLeft s1] | s0 == s1 -> Add s0
    Loop [Dec 1, GoLeft s0, Inc 1, GoRight s1] | s0 == s1 -> Add (- s0)
    Loop [Dec 1, GoRight s0, Dec 1, GoLeft s1] | s0 == s1 -> Sub s0
    Loop [Dec 1, GoLeft s0, Dec 1, GoRight s1] | s0 == s1 -> Sub (- s0)
    Loop
      [Dec 1, GoRight p0, Inc p1, GoRight p2, Inc p3, GoLeft p4]
        | p0 + p2 == p4 ->
          Mul p0 (p0 + p2) p1 p3
    Loop [Dec value] | value > 0 -> Drop
    Loop contents -> Loop $ map optimize contents
    Inc value -> Inc value
    Dec value -> Dec value
    GoLeft value -> GoLeft value
    GoRight value -> GoRight value
    Input value -> Input value
    Output value -> Output value
    Mul a b c d -> Mul a b c d
    Sub a -> Sub a
    Add a -> Add a
    Drop -> Drop
