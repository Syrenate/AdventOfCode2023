import Data.List

getGapCoords :: [String] -> [Int] -- Returns list of line coordinates: positive for horizontal line, negative for vertical line
getGapCoords xs = expH 2 0 (transpose xs) ++ expH 1 0 xs
    where
        expH :: Int -> Int -> [String] -> [Int]
        expH n i [] = []
        expH n i (x:xs) | all (=='.') x = (if n == 1 then i else (-i)) : expH n (i+1) xs
                        | otherwise     = expH n (i+1) xs

getCoords :: [String] -> [(Int,Int)] -- Coordinates of all galaxies (rel. to top left corner)
getCoords xss = concat [[(i,j) | (x,i) <- zip xs [0..], x == '#'] | (xs,j) <- zip xss [0..]]

getSumDist :: Int -> [Int] -> [(Int,Int)] -> Int -- Sum of distance between all coords
getSumDist sepDist gaps xs = sum [sum [abs (i-i') + abs (j-j') + (sepDist - 1) * countGapPasses gaps (i,j) (i',j')| ((i,j),(i',j')) <- zip xs (take (length xs - n) (drop n (cycle xs)))] | n <- [1..(length xs)]]

countGapPasses :: [Int] -> (Int,Int) -> (Int,Int) -> Int -- How many gaps are between a given pair of coordinates
countGapPasses gaps (i,j) (i',j') = sum [if n > 0 then (if n `elem` (if j > j' then [j'..j] else [j..j']) then 1 else 0) else (if (-n) `elem` (if i > i' then [i'..i] else [i..i']) then 1 else 0)| n <- gaps]

main = do input <- readFile "Day11Input.txt"; let inp = lines input
          putStrLn ("Part 1: " ++ show (getSumDist 2 (getGapCoords inp) (getCoords inp)))
          putStrLn ("Part 2: " ++ show (getSumDist 1000000 (getGapCoords inp) (getCoords inp)))