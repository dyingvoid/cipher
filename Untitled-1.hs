data FourLetterAlphabet = L1 | L2 | L3 | L4 
                          deriving(Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a 
rotN alphabetSize char = toEnum rotation
    where halfAlphabet = alphabetSize `div` 2
          offset = fromEnum char + halfAlphabet
          rotation = offset `mod` alphabetSize 

largestCharNumber :: Int
largestCharNumber = fromEnum(maxBound :: Char)

message :: [FourLetterAlphabet]
message = [L1, L2, L3, L4, L1, L1, L2]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] 
                          -> [FourLetterAlphabet]
fourLetterAlphabetEncoder vals = map rot41 vals
    where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
          rot41 = rotN alphaSize

xorBool :: Bool -> Bool -> Bool
xorBool value1 value2 = (value1 || value2) && (not(value1 && value2))

xorBoolPair :: (Bool, Bool) -> Bool
xorBoolPair lst = xorBool (fst lst) (snd lst)

xorCharBits :: [(Bool, Bool)] -> [Bool]
xorCharBits lst = map xorBoolPair lst


xor''' :: ([Bool], [Bool]) -> [Bool]
xor''' pair = xorCharBits (zip (fst pair) (snd pair))

xor' :: [([Bool], [Bool])] -> [[Bool]]
xor' lst = map xor''' lst

toBin :: Int -> [Bool]
toBin 0 = [False]
toBin 1 = [True]
toBin n = if (remainder == 0)
          then False : toBin(nextVal)
          else True : toBin(nextVal)
    where remainder = n `mod` 2
          nextVal = n `div` 2 

maxBits :: Int
maxBits = length (toBin maxBound)

intToBits :: Int -> [Bool]
intToBits n = leadingFalses ++ reversedBits
    where reversedBits = reverse (toBin n)
          missingBits = maxBits - (length reversedBits)
          leadingFalses = take missingBits (cycle[False])

charToBits :: Char -> [Bool]
charToBits char = intToBits (fromEnum char)

stringToBits :: [Char] ->[[Bool]]
stringToBits string = map charToBits string

bitsToInt :: [Bool] -> Int
bitsToInt bits = sum (map (\x -> 2 ^ (snd x)) trueLocations)
    where size = length bits
          indices = [size - 1, size - 2 .. 0]
          trueLocations = filter(\x -> fst x == True)
                                (zip bits indices)

bitsToChar :: [Bool] -> Char
bitsToChar bits = toEnum (bitsToInt bits)

bitsToString :: [[Bool]] -> [Char]
bitsToString bits = map bitsToChar bits

myPad :: [Char]
myPad = "popa"

myPlainText :: [Char]
myPlainText = "Vlad"

applyOTP :: [Char] -> [Char] -> [[Bool]]
applyOTP pad plain = xor' pairBits
    where pairBits = zip padBits plainBits
          padBits = stringToBits pad
          plainBits = stringToBits plain

toStringOTP :: [Char]-> [Char] -> [Char]
toStringOTP pad plain = bitsToString bits
    where bits = applyOTP pad plain

encoderDecoder :: [Char] -> [Char]
encoderDecoder = toStringOTP myPad 