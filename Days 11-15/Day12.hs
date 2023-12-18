{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
getInput :: IO [(String,[Int])]
getInput = do inp <- readFile "Day12Input.txt"
              return ((parseInput . lines) inp)

parseInput :: [String] -> [(String,[Int])]
parseInput [] = []
parseInput (x:xs) = let s = words x
                     in (head s, map read (words (map (\y -> if y == ',' then ' ' else y) (last s)))) : parseInput xs

simplify :: String -> String
simplify [] = []
simplify (x:xs) | x == '.' = '.' : simplify (dropWhile (=='.') xs)
                | otherwise = x : simplify xs

countPoss :: String -> [Int] -> [((String,[Int]),Int)] -> Int
countPoss xs ns memo | (xs,ns) `elem` map fst memo = head [snd m | m <- memo, fst m == (xs,ns)]
                     | null xs = if null ns then 1 else 0
                     | null ns = if '#' `elem` xs then 0 else 1

                     | otherwise = (if head xs == '.' || head xs == '?' then countPoss (tail xs) ns memo else 0) +
                                   (if (head xs == '#' || head xs == '?') && (head ns <= length xs &&
                                                                            '.' `notElem` take (head ns) xs &&
                                                                            (head ns == length xs || xs !! head ns /= '#')) then countPoss (drop (head ns + 1) xs) (tail ns) memo else 0)

part1 :: [(String,[Int])] -> Int
part1 xss = sum [uncurry countPoss x [] | x <- map (\(x,y) -> (simplify x, y)) xss]

unfold :: (String,[Int]) -> (String,[Int])
unfold (xs,ns) = (copy xs 5 "?", copy ns 5 [])
    where
        copy :: [a] -> Int -> [a] -> [a]
        copy xs n x | n == 1 = xs
                    | otherwise = xs ++ x ++ copy xs (n-1) x

part2 :: [(String,[Int])] -> Int
part2 xss = part1 (map unfold xss)

main :: IO ()
main = do inp <- getInput
          putStrLn ("Part 1: " ++ show (part1 inp))
          putStrLn ("Part 2: " ++ show (part2 inp))