module Maxfucktor.AST (Optimized, UnOptimized, AST(..)) where


data Optimized
data UnOptimized


-- AST type is parametrized with a phantom metadata - Optimized/UnOptimized
data AST a = 
    Inc     Int
  | Dec     Int
  | GoLeft  Int
  | GoRight Int
  | Input   Int
  | Output  Int
  | Loop    [AST a]
  | Drop
  | Add     Int
  | Sub     Int
  | Mul     {mulShift0::Int, mulShift1::Int, mulValue0::Int, mulValue1::Int}
  deriving (Show, Eq)
