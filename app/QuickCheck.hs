{-# LANGUAGE TemplateHaskell #-}
module QuickCheck where
import Test.QuickCheck
import Test.QuickCheck.All
import Expr
import REPL
import BinaryTree
import Data.Maybe

instance Arbitrary Expr where
    arbitrary = oneof [return Value Add Value,
                       return ]

instance Arbitrary Value where
    arbitrary = oneof [return IntVal,
                       return FloatVal]


--prop_removeMaybe :: Maybe a -> Bool
--prop_removeMaybe a = isNothing(removeMaybe a) || not isNothing(removeMaybe a)

--prop_Repl_addIntegers :: String -> Bool 
--prop_Repl_addIntegers str = True

prop_Eval :: Expr -> Bool
prop_Eval e = True

return []
runTests = $quickCheckAll