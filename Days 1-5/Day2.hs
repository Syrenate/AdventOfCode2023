{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
import MyParser
import Data.Char

getInput = do xs <- readFile "Day2Input.txt"
              return (getResult (parse sepLines xs))


cleanInput :: String -> String
cleanInput [] = []
cleanInput s | " blue"  `isPrefixOf` s = 'B' : cleanInput (drop 5 s)
             | " red"   `isPrefixOf` s = 'R' : cleanInput (drop 4 s)
             | " green" `isPrefixOf` s = 'G' : cleanInput (drop 6 s)
             | or [[n] `isPrefixOf` s | n <- ['0','1','2','3','4','5','6','7','8','9',';',':']] = head s : cleanInput (tail s)
             | otherwise = cleanInput (tail s)

isPrefixOf :: String -> String -> Bool
isPrefixOf xs ys | length ys < length xs      = False
                 | take (length xs) ys == xs = True
                 | otherwise                   = False


part1 :: [String] -> Int
part1 bags = sum [fst (cleanedInput currBag) | currBag <- bags, not (doesOverflow (snd (cleanedInput currBag)))]
    where
        doesOverflow :: [[(Int,Char)]] -> Bool
        doesOverflow xs = or [or [isOverflow n c | (n,c) <- xs'] | xs' <- xs]
            where
                isOverflow :: Int -> Char -> Bool
                isOverflow n c | c == 'B' = n > 14
                               | c == 'G' = n > 13
                               | c == 'R' = n > 12

part2 :: [String] -> Int
part2 xs = sum [getPower (snd (cleanedInput line)) | line <- xs]
    where
        getPower :: [[(Int,Char)]] -> Int
        getPower xs = getMin 'R' xs' * getMin 'B' xs' * getMin 'G' xs'
            where
                xs' = concat xs

                getMin :: Char -> [(Int,Char)] -> Int
                getMin n is = maximum [x | (x,i) <- is, i == n]


cleanedInput :: String -> (Int,[[(Int,Char)]])
cleanedInput ts = (bagIndex (splitBag ts), bagDraws (splitBag ts))
    where
        bagDraws :: [String] -> [[(Int,Char)]]
        bagDraws xs = getResult (parse parseInput2 (cleanInput (last xs)))
        bagIndex :: [String] -> Int
        bagIndex xs = read (filter isDigit (head xs))

        splitBag :: String -> [String]
        splitBag = wordsWhen (==':')

main = do input <- getInput
          print (part1 input)
          print (part2 input)