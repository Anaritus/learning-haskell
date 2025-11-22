pairsum :: [Int] -> [Int]
pairsum [] = []
pairsum [x] = [x]
pairsum (x : y : tail) = (x + y) : pairsum tail

main = print (pairsum [1, 3, 4, 5])
