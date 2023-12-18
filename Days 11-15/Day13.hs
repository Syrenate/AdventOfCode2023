import Data.List

getInput = do inp <- readFile "Day13Input.txt"
              return (lines inp)

parseInput :: [String] -> [[String]]
parseInput [] = []
parseInput xs = ns : parseInput (drop (length ns + 1) xs)
    where ns = takeWhile (/=[]) xs

symmetry :: Int -> [String] -> [Int]
symmetry n xs | n+1 >= length xs = [-1]
              | (xs !! n) == (xs !! (n+1)) && isSymmetric 0 n xs = n+1 : symmetry (n+1) xs
              | otherwise = symmetry (n+1) xs

isSymmetric :: Int -> Int -> [String] -> Bool
isSymmetric off n xs | (n+1+off) >= length xs || (n-off) < 0 = True
                     | xs !! (n + 1 + off) == xs !! (n - off) = isSymmetric (off+1) n xs
                     | otherwise = False

part1 :: [[String]] -> [[(Char,Int)]]
part1 xss = [[('H',x) | x <- filter (>= 0) (symmetry 0 xs)] ++ [('V',x) | x <- filter (>= 0) (symmetry 0 (transpose xs))] | xs <- xss]

genPossibilities :: [String] -> [[String]]
genPossibilities xss = [vary i j | i <- [0..(length (head xss) - 1)], j <- [0..(length xss - 1)]]
    where
        vary i j = take j xss ++ [take i (xss !! j) ++ [if (xss !! j) !! i == '.' then '#' else '.'] ++ drop (i+1) (xss !! j)] ++ drop (j+1) xss

part2 xss = [(head . removeDupes) (filter (not. null) [filter (/=y) t | t <- removeDupes (filter (not . null) (part1 xs))]) | (xs,y) <- zip (map genPossibilities xss) (map head (part1 xss))]

removeDupes :: Eq a => [a] -> [a]
removeDupes [] = []
removeDupes (x:xs) | x `elem` xs = removeDupes xs
                   | otherwise   = x : removeDupes xs

sumLines :: [(Char,Int)] -> Int
sumLines [] = 0
sumLines ((c,x):xs) | c == 'V' = x + sumLines xs
                    | c == 'H' = 100 * x + sumLines xs

main :: IO ()
main = do inp <- getInput
          let xss = parseInput inp
          putStrLn ("Part 1: " ++ show  (sumLines (map head (part1 xss))))
          putStrLn ("Part 2: " ++ show  (sumLines (map head (part2 xss))))