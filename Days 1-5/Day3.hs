import MyParser
import Data.Char

-- Useful funcs
getInput :: IO [[Char]]
getInput = do input <- readFile "Day3Input.txt"
              return (getResult (parse sepLines input))

removeDupes :: Eq a => [a] -> [a]
removeDupes [] = []
removeDupes (x:xs) | null xs      = [x]
                      | x `elem` xs = removeDupes xs
                      | otherwise    = x : removeDupes xs

getCoords :: String -> [String] -> [(Int,Int)]
getCoords n xss = concat [[(i,j) | (x,i) <- zip xs [0..], if n == "num" then isDigit x else x == '*'] | (xs,j) <- zip xss [0..]]
-- ============== --


-- Part 1 Funcs
getPartCoord :: Int -> (Int,Int) -> [String] -> (Int,Int)
getPartCoord n (i,j) xss | n == 0 = if (i-1,j) `elem` cs then getPartCoord (-1) (i-1,j) xss else (i,j)
                         | n == -1 = if (i-1,j) `elem` cs then getPartCoord (-1) (i-1,j) xss else (i,j)
    where
        cs = getCoords "num" xss

getPartNum :: (Int,Int) -> [String] -> String
getPartNum (i,j) xss = val : if (i+1,j) `elem` cs then getPartNum (i+1,j) xss else ""
    where
        val = (xss !! j) !! i
        cs = getCoords "num" xss

part1 :: [String] -> Int
part1 xs = sum [read (getPartNum x xs) | x <- removeDupes [getPartCoord 0 c xs | c <- getCoords "num" xs, isPartNum c xs]]
    where
        isPartNum :: (Int,Int) -> [String] -> Bool
        isPartNum (i,j) xss = or [ or [not (isDigit x) && (x /= '.') | (x,i') <- zip xs [0..], i' `elem` [i-1,i,i+1]] | (xs,j') <- zip xss [0..], j' `elem` [j-1,j,j+1]]
-- ============== --


-- Part 2 Funcs --
getGearAdjs :: (Int,Int) -> [String] -> [(Int,Int)]
getGearAdjs (i,j) xss = concat [[(i',j') | (x,i') <- zip xs [0..], i' `elem` [i-1,i,i+1], isDigit x] | (xs,j') <- zip xss [0..], j' `elem` [j-1,j,j+1]]

getGearRatio :: [(Int,Int)] -> [String] -> [Int]
getGearRatio cs xss = let coords = removeDupes [getPartCoord 0 c xss | c <- cs]
                       in if length coords == 2 then [read (getPartNum c xss) | c <- coords] else []

part2 :: [String] -> Int
part2 xs = sum [product (if null (nums c) then [0] else nums c) | c <- getCoords "" xs]
    where
        nums :: (Int,Int) -> [Int]
        nums c = getGearRatio (getGearAdjs c xs) xs
-- ============== --


-- Main Call --
main :: IO ()
main = do xs <- getInput;
          print (part1 xs);
          print (part2 xs);
-- ============== --