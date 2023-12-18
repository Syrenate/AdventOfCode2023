{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Data.Char

getInput :: IO [String]
getInput = do inp <- readFile "Day12Input.txt"
              return (lines inp)

parseInput :: [String] -> [(String,[Int])]
parseInput [] = []
parseInput (x:xs) = let s = words x
                     in (head s, map read (words (map (\y -> if y == ',' then ' ' else y) (last s)))) : parseInput xs


recurseThrough ::String -> [Int] -> [String]
recurseThrough [] [] = [""]
recurseThrough [] _  = []
recurseThrough xs [] | '#' `elem` xs = []
                     | otherwise = [map (const '.') xs]
recurseThrough f@(x:xs) ns | x == '?' = recurseThrough ('.':xs) ns ++ recurseThrough ('#':xs) ns
                                | x == '#' = if '.' `notElem` take (head ns) f then map (['#' | x <- [1..(head ns)]]++) (recurseThrough (drop (head ns) f) (tail ns)) else []
                                | x == '.' = map ('.':) (recurseThrough xs ns)

arrCount :: Int -> Int -> String -> [Int] -> Int -- spring index, group index, spring record, groups
arrCount i n xs ns | n >= length ns = if i < length xs && '#' `elem` drop i xs then 0 else 1
                   | i >= length xs = 0
                   | xs !! i == '.' = arrCount (i+1) n xs ns
                   | xs !! i == '#' = if '.' `notElem` take (ns !! n) (drop i xs) && xs !! (i + (ns !! n)) /= '#' then arrCount (i+(ns !! n)) (n+1) xs ns else 0
                   | xs !! i == '?' = if '.' `notElem` take (ns !! n) (drop i xs) && xs !! (i + (ns !! n)) /= '#' 
                                      then arrCount (i+1) n xs ns + arrCount (i+(ns !! n)) (n+1) xs ns else arrCount (i+1) n xs ns

editStr :: String -> Int -> Char -> String
editStr xs i x = take i xs ++ [x] ++ drop (i+1) xs

t1 = "???.###"
t2 = [1,1,3] :: [Int]

getPoss :: String -> [Int] -> [String]
getPoss xs ns = removeDupes (filter (\x -> length (words (map (\y -> if y == '.' then ' ' else y) x)) == length ns && length x == length xs) (recurseThrough xs ns))

removeDupes :: Eq a => [a] -> [a]
removeDupes [] = []
removeDupes (x:xs) | x `elem` xs = removeDupes xs
                   | otherwise = x : removeDupes xs

part1 :: [(String,[Int])] -> [Int]
part1 xs = [length (getPoss x ns) | (x,ns) <- xs]

part2 :: [(String,[Int])] -> [Int]
part2 xs = let ys = map (\(str,ns) -> (copy str "?" 5,copy ns [] 5)) xs
            in part1 ys


copy :: [a] -> [a] -> Int -> [a]
copy xs s n = concat [xs ++ s | x <- [1..(n-1)]] ++ xs

main = do inp <- getInput
          let n = parseInput inp
          print (part1 n)
          print (part2 n)