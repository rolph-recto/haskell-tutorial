-- ReversePolish.hs
-- Reverse Polish arithmetic expression calculator

import Data.Char
import Control.Monad.State
import EitherT

type Stack a = [a]

data Operator = Add | Sub | Mul | Div
data Token = Lit Int | Op Operator

lget = lift get
lput = lift . put

doOperation :: Operator -> Int -> Int -> Either String Int 
doOperation Add x y = Right $ x + y
doOperation Sub x y = Right $ x - y
doOperation Mul x y = Right $ x * y
doOperation Div _ 0 = Left "Division by zero!"
doOperation Div x y = Right $ x `div` y

-- evaluate an arithmetic expression in Reverse Polish form
evalReversePolish :: [Token] -> EitherT String (State (Stack Int)) Int 
-- if there are no tokens left to evaluate, then there should only be 
-- exactly one value in the stack
evalReversePolish [] = do
  s <- lget
  case compare (length s) 1 of
    LT -> do
      failEitherT "Input has not enough numbers!"
    GT -> do
      failEitherT "Input has too many numbers!"
    EQ -> do
      return $ head s 

evalReversePolish (x:xs) = case x of
  Lit n -> do
    s <- lget
    lput $ n:s
    evalReversePolish xs

  Op op -> do
    s <- lget
    if length s < 2
    then failEitherT "Operator does not have enough operands!"
    else do
      let (x,y) = (head s, head $ tail s)
      lput $ tail $ tail s
      case doOperation op x y of
        Left err -> failEitherT err
        Right n -> do
          s' <- lget
          lput $ n:s'
          evalReversePolish xs

charToOperator :: Char -> Either String Token
charToOperator '+' = Right $ Op Add
charToOperator '-' = Right $ Op Sub
charToOperator '*' = Right $ Op Mul
charToOperator '/' = Right $ Op Div 
charToOperator op  = Left $ "Unknown symbol " ++ [op]

stringToToken :: String -> Either String Token
stringToToken [s] = do
  if isDigit s
  then return $ Lit (read [s] :: Int)
  else charToOperator s
stringToToken s@(_:_) = Left $ "Unknown symbol " ++ s

parseExpr :: String -> Either String [Token]
parseExpr e = forM (words e) stringToToken

calcExpr :: String -> String
calcExpr e = case parseExpr e of
  Left err -> err
  Right tokens -> 
    case evalState (runEitherT $ evalReversePolish tokens) [] of
      Left err -> err
      Right n -> show n

main = do
  input <- getLine
  putStrLn $ calcExpr input
  main

