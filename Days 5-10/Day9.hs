getInput :: IO [[Int]]
getInput = do inp <- readFile "Day9Input.txt"
              return [map read x | x <- map words (lines inp)]

getSequence :: [Int] -> [[Int]]
getSequence xs = if all (== head xs) xs then [xs] else xs : getSequence [y-x | (x,y) <- take (length xs - 1) (zip xs (drop 1 (cycle xs)))]

part1 :: [[Int]] -> Int
part1 xss = sum [sum [last ys | ys <- getSequence xs] | xs <- xss]

part2 :: [[Int]] -> Int
part2 xss = sum [foldr (-) 0 [ head ys | ys <- getSequence xs] | xs <- xss]

main :: IO ()
main = do inp <- getInput
          print (part1 inp)
          print (part2 inp)