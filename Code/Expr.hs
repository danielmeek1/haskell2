module Expr where

import Parsing
import System.Console.Haskeline

type Name = String



-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          | ToString Expr
          | Val Value
          | Var Name --variable name
          | Concat Value Value
          
  deriving Show

data Value = IntVal Int | StrVal String

instance Show Value where
  show (IntVal a) = show a
  show (StrVal a) = a

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Quit
  deriving Show

eval :: [(Name, Value)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x -- for values, just give the value directly
eval [] (Var x) = Nothing
eval vars (Var x) 
                | hasVar vars x = Just (snd ((filter (\(n,v) -> n==x) vars)!!0))
                | otherwise = Nothing--returns value of a variable
eval vars (Add x y) = case eval vars x of
                        Just (IntVal x) -> case eval vars y of
                                    Just (IntVal y) -> Just (IntVal (x + y))
                                    _ -> Nothing
                        _ ->  Nothing 
eval vars (Sub x y) = case eval vars x of
                        Just (IntVal x) -> case eval vars y of
                                    Just (IntVal y) -> Just (IntVal (x - y))
                                    _ -> Nothing
                        _ ->  Nothing 
eval vars (Mult x y) = case eval vars x of
                        Just (IntVal x) -> case eval vars y of
                                    Just (IntVal y) -> Just (IntVal (x * y))
                                    _ -> Nothing
                        _ ->  Nothing 
eval vars (Div x y) = case eval vars x of
                        Just (IntVal x) -> case eval vars y of
                                    Just (IntVal y) -> Just (IntVal (x `div` y))
                                    _ -> Nothing
                        _ ->  Nothing 
eval vars (ToString x) = ((eval vars x))

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

digitsToInt :: [Char] -> Int
digitsToInt (d:ds) = (foldl (\ x y ->10*x + y) 0 (map digitToInt (d:ds)))

concatenate :: Expr -> Expr -> Value
concatenate (Val (StrVal s1)) (Val (StrVal s2)) = ( (StrVal(s1 ++ s2)))


hasVar ::  [(Name, Value)] -> Name  -> Bool
hasVar [] n = False
hasVar ((n,_):vars) name 
               | name == n = True
               | otherwise = hasVar vars name

pCommand :: Parser Command
pCommand = do t <- letter
              ts <- many letter
              space
              char '='
              space
              e <- pExpr
              return (Set (t:ts) e)
            ||| do string "print"
                   space
                   e <- pExpr
                   return (Print e)
            ||| do string "quit"
                   return Quit


pExpr :: Parser Expr
pExpr = do t <- pTerm
           do space
              string "++"
              space 
              e <- pExpr
              return (Val (concatenate t e))
            |||do  space
                   char '+'
                   space
                   e <- pExpr
                   return (Add t e)
            ||| do space
                   char '-'
                   space
                   e <- pExpr
                   return (Sub t e)
            ||| do space
                   char '"'
                   e <- (many (alphanum ||| oneSpace))
                   char '"'
                   space
                   return (Val (StrVal e)) 
                 ||| return t
            
        {- ||| do string "input"
                do inp <- input
                   if all isDigit inp then
                     return (Val (digitsToInt inp)) else
                       error "string not implemented yet"-}


pFactor :: Parser Expr
pFactor = do d <- digit
             ds <- many digit
             return (Val (IntVal (digitsToInt (d:ds))))
           ||| do v <- many letter
                  return (Var v)
                ||| do space
                       char '('
                       space
                       e <- pExpr
                       space
                       char ')'
                       return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do space
              char '*'
              space
              e <- pTerm
              space
              return (Mult f e)
            ||| do space
                   char '/'
                   space
                   e <- pTerm
                   space
                   return (Div f e)
                 ||| return f

input :: IO String
input =  runInputT defaultSettings input
   where
      input :: InputT IO String
      input = do
         usrinput <- getInputLine ""
         case usrinput of
            Nothing -> return []
            Just given -> return given



