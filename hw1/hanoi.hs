import Text.Printf (printf)

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
      let k = optimum n
       in fourHanoi (n - k) a c d b
            ++ hanoi k a d c
            ++ fourHanoi (n - k) b a c d

main = do
  let p3 = length (hanoi 15 "" "" "")
      p4 = length (fourHanoi 15 "" "" "" "")
  printf "For 15 disks, 3 pegs require %d moves, while 4 pegs require %d moves\n" p3 p4
