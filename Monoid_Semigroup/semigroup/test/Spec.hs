import Test.QuickCheck
import Data.Semigroup
import Lib

instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

main :: IO()
main = quickCheck (semigroupAssoc :: TrivAssoc)
