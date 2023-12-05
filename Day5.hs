{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import MyParser
import Data.Char

getInput = do input <- readFile "Day5Input.txt"
              return (getResult (parse sepEmpLines input))

              
-- Useful Funcs
getSeeds :: String -> [Int]
getSeeds ('s':'e':'e':'d':'s':':':xs) = map read (wordsWhen (==' ') xs)

mapSeeds :: [Int] -> [((Int,Int),Int)]-> [Int]
mapSeeds [] _ = []
mapSeeds (s:ss) xss | null ts = s : mapSeeds ss xss
                    | otherwise = head ts : mapSeeds ss xss
    where
        ts = [s - x + y | ((x,y),z) <- xss, s >= x && s <= x+z]

getMaps :: [String] -> [[((Int,Int),Int)]]
getMaps inp = go inp (-1) 0
    where
        go :: [String] -> Int -> Int -> [[((Int,Int),Int)]] -- input -> index of last split -> current index -> result
        go [] n m = []
        go (x:xs) n m | ':' `elem` x = [stringToMap y | y <- drop (n+1) (take m inp)] : go xs m (m+1)
                      | otherwise    = go xs n (m+1)

        stringToMap :: String -> ((Int,Int),Int)
        stringToMap xs = let ns = map read (wordsWhen (==' ') xs)
                          in ((ns !! 1,head ns),ns !! 2)

        mapSeeds :: [Int] -> [((Int,Int),Int)]-> [Int]
        mapSeeds [] _ = []
        mapSeeds (s:ss) xss | null ts = s : mapSeeds ss xss
                            | otherwise = head ts : mapSeeds ss xss
            where
                ts = [s - x + y | ((x,y),z) <- xss, s >= x && s <= x+z]


-- Part 1 Funcs --
part1 :: [String] -> Int
part1 xs = minimum (transformSeeds (getMaps (drop 2 xs)) (getSeeds (head xs)))
    where
        transformSeeds :: [[((Int,Int),Int)]] -> [Int] -> [Int]
        transformSeeds xss ns = foldl mapSeeds ns xss


-- Part 2 Funcs --
findMin :: Int -> Int -> [[((Int,Int),Int)]] -> [(Int,Int)] -> Int
findMin _ 3979038278 _ _ = 3979038278
findMin step n xss src | not (isInRange (head (destToSource n xss)) src) = findMin step (n+step) xss src
                       | step /= 1 = findMin 1 (n-1001) xss src
                       | otherwise = n
    where
        revList :: [a] -> [a]
        revList [] = []
        revList (x:xs) = revList xs ++ [x]

        revMap :: [((Int,Int),Int)] -> [((Int,Int),Int)]
        revMap [] = []
        revMap (((x,y),z):xs) = revMap xs ++ [((x,y),z)]

        destToSource :: Int -> [[((Int,Int),Int)]] -> [Int]
        destToSource n xss = foldl mapSeeds [n] (revList (map revMap xss))

        isInRange :: Int -> [(Int,Int)] -> Bool
        isInRange n [] = False
        isInRange n ((x,y):xs) = (n >= x && n <= y) || isInRange n xs

ranges :: [Int] -> [(Int,Int)]
ranges [] = []
ranges (x:y:xs) = (x,x+y) : ranges xs

part2 :: [String] -> Int
part2 xs = findMin 1000 0 (getMaps (drop 2 xs)) ((ranges . getSeeds) (head xs)) - 1


-- Main Call --
main :: IO ()
main = do inp <- getInput 
          print (part1 inp)
          print (part2 inp)