import Test.QuickCheck
import Data.Semigroup
import Lib

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: Trivial -> Trivial -> Trivial -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
    quickCheck (semigroupAssoc)
