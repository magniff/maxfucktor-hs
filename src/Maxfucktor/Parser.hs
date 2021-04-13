{-# LANGUAGE TupleSections #-}


module Maxfucktor.Parser where


import Control.Applicative ( Alternative(some, many, (<|>)) )
import Data.Maybe ( isJust )


data AST = 
    Inc     Int
  | Dec     Int
  | GoLeft  Int
  | GoRight Int
  | Input   Int
  | Output  Int
  | Loop    [AST]
  deriving (Show, Eq)


-- i : input type
-- o : output type
newtype Parser i o = Parser {runParser :: [i] -> (Maybe o, [i])}


instance Functor (Parser input) where
  fmap f parser = 
    Parser $ \input ->
      let (result, input_rest) = runParser parser input in
        (f <$> result, input_rest)


instance Applicative (Parser input) where
  pure a = Parser (Just a,)
  pf <*> pa = 
    Parser $ \input ->
      let (f, input') = runParser pf input in
        let (a, input'') = runParser pa input' in
          (
            do
              func <- f
              value <- a
              Just $ func value,
            input''
          )


instance Monad (Parser input) where
  pa >>= f =
    Parser $ \input ->
      let (a, input') = runParser pa input in
        case a of
          Nothing -> (Nothing, input')
          Just value -> runParser (f value) input'


instance Alternative (Parser input) where
  p0 <|> p1 =
    Parser $ \input ->
      let res@(r0, input') = runParser p0 input in
        case r0 of
          Just value -> res
          Nothing -> runParser p1 input


p :: (a -> Bool) -> Parser a a
p f = 
  Parser $ \input ->
    let fail = (Nothing, input) in
    if null input
      then fail
      else
        if f $ head input
          then (Just $ head input, tail input)
          else fail


-- BF loop [%code-here%]
loopBody :: Parser Char AST
loopBody = do
  p (== '[')
  match <- many code
  p (== ']')
  return $ Loop match


-- any BF code
code :: Parser Char AST
code =
  foldl (<|>) loopBody $
  map 
    (\(sym, ast) -> ast . length <$> some (p (== sym)))
    [
      ('+', Inc),
      ('-', Dec),
      ('>', GoRight),
      ('<', GoLeft),
      ('.', Output),
      (',', Input)
    ]


noLeftOvers :: Eq b => Parser a b -> Parser a b
noLeftOvers p=
  Parser $ \input ->
    let res@(inner_result, leftover) = runParser p input in
      if isJust inner_result && null leftover
        then res
        else (Nothing, leftover)


wholeProgram :: Parser Char [AST]
wholeProgram = noLeftOvers $ many code
