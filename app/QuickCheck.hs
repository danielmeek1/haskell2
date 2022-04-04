{-# LANGUAGE TemplateHaskell #-}
module QuickCheck where
import Test.QuickCheck
import Test.QuickCheck.All
import Expr
import REPL
import BinaryTree
import Data.Maybe


-- Check digitsToInt 
--prop_removeMaybe :: Maybe a -> Bool
--prop_removeMaybe a = isNothing(removeMaybe a) || not isNothing(removeMaybe a)


--runTests = $quickCheckAll