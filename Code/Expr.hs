module Expr where
import Parsing
import System.Console.Haskeline

import Data.Char ( isDigit )
import BinaryTree
type Name = String



-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add      Expr Expr
          | Sub      Expr Expr
          | Mult     Expr Expr
          | Div      Expr Expr
          | Abs      Expr          --find the absolute value of a number
          | Pow      Expr Expr
          | Mod      Expr Expr
          | ToString Expr          --convert a number to a string
          | ToInt    Expr          --convert a string to an integer 
          | Val      Value         --a value
          | Var      Name          --variable name
          | Input
  deriving Show

instance Show Value where
  show (IntVal a)    = show a
  show (FloatVal a)  = show a
  show (StrVal a)    = a
  show (Error a)     = a

-- These are the REPL commands
data Command = Set          Name Expr     -- assign an expression to a variable name
             | Print        Expr          -- evaluate an expression and print the result
             | File         String        -- load a file and execute commands within it.
             | Quit                       -- Exit the system
             | NoCommand                  -- user did not enter a command
  deriving Show


data Value = IntVal         Int
           | FloatVal       Float
           | StrVal         String
           | Error          String

newtype Variable = Variable (Name,Value)

instance Eq Variable where
       (==) (Variable (n1,_)) (Variable (n2,_))         = n1==n2

instance Ord Variable where
       compare (Variable (n1,_)) (Variable (n2,_))      = compare n1 n2


eval :: BTree Variable -> -- Variable name to value mapping
                   Expr -> -- Expression to evaluate
            Maybe Value -- Result (if no errors such as missing variables)
eval vars (Val x)    = Just x -- for values, just give the value directly
eval vars Input      = Just (Error "cannot evaluate input that has not been given")

{-
       Evaluate value of variable
-}
eval vars (Var x) = case getElem vars (Variable (x,IntVal 0)) of
                     Just (Variable (_,v))       -> Just v
                     _                           -> Nothing
{-
       Evaluate addition
-}
eval vars (Add x y) = case (eval vars x, eval vars y) of
                            (Just (IntVal x),Just (IntVal y))         -> Just (IntVal (x + y))
                            (Just (IntVal x),Just (FloatVal y))       -> Just (FloatVal (fromIntegral x + y))
                            (Just (FloatVal x),Just (IntVal y))       -> Just (FloatVal (x + fromIntegral y))
                            (Just (FloatVal x),Just (FloatVal y))     -> Just (FloatVal (x + y))
                            (Just (StrVal x),Just (StrVal y))         -> Just (StrVal (x ++ y))
                            (Just (Error e),_)                        -> Just (Error e)
                            (_,Just (Error e))                        -> Just (Error e)
                            _                 -> Just (Error "'+' operator must be used between two numbers or two strings")

{-
       Evaluate subtraction
-}
eval vars (Sub x y) = case (eval vars x, eval vars y) of
                            (Just (Error e),_)                        -> Just (Error e)
                            (_,Just (Error e))                        -> Just (Error e)
                            (Just (IntVal x),Just (IntVal y))         -> Just (IntVal (x - y))
                            (Just (IntVal x),Just (FloatVal y))       -> Just (FloatVal (fromIntegral x - y))
                            (Just (FloatVal x),Just (IntVal y))       -> Just (FloatVal (x - fromIntegral y))
                            (Just (FloatVal x),Just (FloatVal y))     -> Just (FloatVal (x - y))
                            _                                         -> Just (Error "'-' operator must be used between two numbers")

{-
       Evaluate multiplication
-}
eval vars (Mult x y) = case (eval vars x, eval vars y) of
                            (Just (IntVal x),Just (IntVal y))         -> Just (IntVal (x * y))
                            (Just (IntVal x),Just (FloatVal y))       -> Just (FloatVal (fromIntegral x * y))
                            (Just (FloatVal x),Just (IntVal y))       -> Just (FloatVal (x * fromIntegral y))
                            (Just (FloatVal x),Just (FloatVal y))     -> Just (FloatVal (x * y))
                            (Just (Error e),_)                        -> Just (Error e)
                            (_,Just (Error e))                        -> Just (Error e)
                            _                                         -> Just (Error "'*' operator must be used between two numbers")


{-
       Evaluate division
-}
eval vars (Div x y) = case (eval vars x, eval vars y) of

                            (Just (IntVal x),Just (IntVal y))         -> Just (IntVal (x `div` y))
                            (Just (IntVal x),Just (FloatVal y))       -> Just (FloatVal (fromIntegral x / y))
                            (Just (FloatVal x),Just (IntVal y))       -> Just (FloatVal (x / fromIntegral y))
                            (Just (FloatVal x),Just (FloatVal y))     -> Just (FloatVal (x / y))
                            (Just (Error e),_)                        -> Just (Error e)
                            (_,Just (Error e))                        -> Just (Error e)
                            _                                         -> Just (Error "'/' operator must be used between two numbers")

{-
       Evaluate |...| (Abs)
-}
eval vars (Abs e) = case eval vars e of
                     Just (IntVal y)      -> Just (IntVal (abs y))
                     Just (FloatVal y)    -> Just (FloatVal (abs y))
                     Just (Error e)       -> Just (Error e)
                     _                    -> Just (Error "'|...|' operator must be used on a number")

{-
       Evaluate ^ (Power)
-}
eval vars (Pow x y) = case (eval vars x, eval vars y) of
                            (Just (IntVal x),Just (IntVal y))         -> Just (IntVal (x ^ y))
                            (Just (IntVal x),Just (FloatVal y))       -> Just (FloatVal (fromIntegral x ** y))
                            (Just (FloatVal x),Just (IntVal y))       -> Just (FloatVal (x ** fromIntegral y))
                            (Just (FloatVal x),Just (FloatVal y))     -> Just (FloatVal (x ** y))
                            (Just (Error e),_)                        -> Just (Error e)
                            (_,Just (Error e))                        -> Just (Error e)
                            _                                         -> Just (Error "'^' operator must be used between two numbers")

{-
       Evaluate % (Mod)
-}
eval vars (Mod x y) = case (eval vars x, eval vars y) of
                            (Just (IntVal x),Just (IntVal y))         -> Just (IntVal (x `mod` y))
                            (Just (Error e),_)                        -> Just (Error e)
                            (_,Just (Error e))                        -> Just (Error e)
                            _                                         -> Just (Error "'%' operator must be used between two Integers")

{-
       Evaluate ToString
-}
eval vars (ToString x) = Just (StrVal (show (removeMaybe(eval vars x))))

{-
       Evaluate ToInt
-}
eval vars (ToInt x) = case eval vars x of
                        Just (StrVal s) -> if all isDigit s then
                           Just (IntVal (digitsToInt s)) else
                             Nothing
                        _ -> Nothing


digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

digitsToInt :: [Char] -> Int
digitsToInt [] = error "No numbers given"
digitsToInt ds = read ds ::Int

digitsToFloat :: [Char] -> Float
digitsToFloat [] = error "No numbers given"
digitsToFloat ds = read ds ::Float


hasVar ::  [(Name, Value)] -> Name  -> Bool
hasVar [] n                = False
hasVar ((n,_):vars) name
               | name == n = True
               | otherwise = hasVar vars name

pCommand :: Parser Command
pCommand = do t <- letter
              ts <- many letter
              space
              char '='
              space
              Set (t:ts) <$> pExpr

            ||| do string "print"
                   space
                   Print <$> pExpr

            ||| do string "quit"
                   return Quit

            ||| do string "file"
                   space
                   char '\0'
                   t <- many printable
                   char '\0'
                   return (File t)

            ||| do string ""
                   return NoCommand


pExpr :: Parser Expr
pExpr = do t <- pTerm
           do  space
               char '+'
               space
               Add t <$> pExpr
            ||| do space
                   char '-'
                   space
                   Sub t <$> pExpr
            ||| do space
                   char '|'
                   space
                   e <- pExpr
                   space
                   char '|'
                   return (Abs e)
                 ||| return t


pFactor :: Parser Expr
pFactor = do string "input"
             return Input

         ||| do d <- digit ||| char '-'
                ds <- many digit
                char '.'
                n <- digit
                ns <- many digit
                return (Val (FloatVal (digitsToFloat ( (d:ds) ++ "." ++ (n:ns) ) )))

         ||| do d <- digit ||| char '-'
                ds <- many digit
                return (Val (IntVal (digitsToInt (d:ds))))

         ||| do  char '('
                 e <- pExpr
                 char ')'
                 return e

         ||| do space
                char '\0'
                e <- many printable
                char '\0'
                space
                return (Val (StrVal e))

         ||| do string "toString("
                space
                e <- pExpr
                space
                char ')'
                return(ToString e)

         ||| do string "toInt("
                space
                e <- pExpr
                space
                char ')'
                return(ToInt e)



         ||| do v <- many letter
                return (Var v)

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
            ||| do space
                   char '%'
                   space
                   Mod f <$> pTerm
            ||| do char '^'
                   space
                   Pow f <$> pExpr

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

removeMaybe :: Maybe a ->  a
removeMaybe (Just a) = a
removeMaybe Nothing  = error "No value"