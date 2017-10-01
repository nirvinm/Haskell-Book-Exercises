import qualified SimpleArithmeticTest as Arith
import qualified IdempotenceTest as Idem
import Test.QuickCheck

main :: IO ()
main = do
    quickCheck Arith.prop_halfIdentity
    quickCheck Arith.prop_stringListOrdered
    quickCheck Arith.plusAssociative
    quickCheck Arith.plusCommutative
    quickCheck Arith.productAssociative
    quickCheck Arith.productCommutative
    quickCheck Arith.prop_quotRem
    quickCheck Arith.prop_divMod
    quickCheck Arith.prop_powerNotAssociative
    quickCheck Arith.prop_powerNotCommutative
    quickCheck Arith.prop_listDoubleReverse
    quickCheck Arith.prop_dollarFunction
    quickCheck Arith.prop_compositionFunction
    quickCheck Arith.prop_cons
    quickCheck Arith.f
    quickCheck Arith.f'
    quickCheck Idem.f
    quickCheck Idem.f'
