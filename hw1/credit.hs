toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x : tail)
  | even (length tail) = x : doubleEveryOther tail
  | otherwise = 2 * x : doubleEveryOther tail

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : tail)
  | x >= 10 = sumDigits (toDigits x) + sumDigits tail
  | otherwise = x + sumDigits tail

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

main = print (validate 4012888888881881)
