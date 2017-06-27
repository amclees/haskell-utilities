import Data.Char

type Cipher = Int -> [Char] -> [Char]

additiveOperation :: Int -> Char -> Char
additiveOperation key char = chr((((ord char) - 97 + key) `mod` 26) + 97)
additiveCipher :: Cipher
additiveCipher key = (\string -> map (additiveOperation key) string)

multiplicitiveOperation :: Int -> Char -> Char
multiplicitiveOperation key char = chr(((((ord char) - 97) * key) `mod` 26) + 97)
multiplicitiveCipher :: Cipher
multiplicitiveCipher key = (\string -> map (multiplicitiveOperation key) string)

affineCipher :: Cipher
affineCipher key = (\string -> map ((additiveOperation key) . (multiplicitiveOperation key)) string)