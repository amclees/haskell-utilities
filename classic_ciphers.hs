type Cipher = Int -> [Char] -> [Char]

monoalphabeticCipher :: (Char -> Char) -> Cipher
monoalphabeticCipher operator = (\key -> map (operator key))

additiveCipher :: Cipher
additiveCipher = monoalphabeticCipher (+) . (`mod` 26)

multiplicativeCipher :: Cipher
multiplicativeCipher = monoalphabeticCipher (*) . (`mod` 26)

affineCipher :: Cipher
affineCipher = additiveCipher . multiplicativeCipher