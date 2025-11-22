type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

optimum :: Integer -> Integer
optimum a = truncate (sqrt (fromInteger (2 * a)))

fourHanoi :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
fourHanoi n a b c d
  | n < 3 = hanoi n a d b
  | otherwise =
      fourHanoi (n - optimum n) a c d b
        ++ hanoi (optimum n) a d c
        ++ fourHanoi (n - optimum n) b a c d

main = print (length (fourHanoi 15 "a" "b" "c" "d"))
