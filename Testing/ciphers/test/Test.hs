{-# LANGUAGE TypeSynonymInstances #-}

import VigenereCipher
import Data.List
import Test.QuickCheck

genAlphabet :: Gen Alphabet
genAlphabet = oneof $ map (\k -> return $ Alphabet k) $ [' ', 'A'..'Z']

instance Arbitrary Alphabet where 
    arbitrary = genAlphabet

prop_VignereCipher :: [Alphabet] -> [Alphabet] -> Bool
prop_VignereCipher x key = decrypt (encrypt x key) key == x

main :: IO ()
main = do
    quickCheck prop_VignereCipher