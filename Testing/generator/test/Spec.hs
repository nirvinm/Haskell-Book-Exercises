import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)
import Lib

-- 1. Equal probabilities for each.
foolGen :: Gen Fool
foolGen = do
    oneof [return Frue, return Fulse]

-- 2. 2/3s chance of Fulse, 1/3 chance of Frue.
foolGen' :: Gen Fool
foolGen' = do
    frequency [(1, return Frue),
               (2, return Fulse)]

main :: IO ()
main = putStrLn "Test suite not yet implemented"
