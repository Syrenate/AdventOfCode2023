import MyParser
import Data.Char

getInput :: IO String
getInput = do readFile "Day1Input.txt"

digitNames = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
digitVals  = ['1','2','3','4','5','6','7','8','9']

toDigit :: Int -> String -> String
toDigit _ [] = []
toDigit n s | or [[n] `isPrefixOf` s | n <- digitVals] = head s : toDigit n (tail s)
			| n==2 && "one" `isPrefixOf` s   = '1' : toDigit n (drop 2 s)
			| n==2 && "two" `isPrefixOf` s   = '2' : toDigit n (drop 2 s)
			| n==2 && "three" `isPrefixOf` s = '3' : toDigit n (drop 4 s)
			| n==2 && "four" `isPrefixOf` s  = '4' : toDigit n (drop 3 s)
			| n==2 && "five" `isPrefixOf` s  = '5' : toDigit n (drop 3 s)
			| n==2 && "six" `isPrefixOf` s   = '6' : toDigit n (drop 2 s)
			| n==2 && "seven" `isPrefixOf` s = '7' : toDigit n (drop 4 s)
			| n==2 && "eight" `isPrefixOf` s = '8' : toDigit n (drop 4 s)
			| n==2 && "nine" `isPrefixOf` s  = '9' : toDigit n (drop 3 s)
			| otherwise = toDigit n $ tail s

isPrefixOf :: String -> String -> Bool
isPrefixOf xs' xs | length xs < length xs'      = False
				  | take (length xs') xs == xs' = True
				  | otherwise                   = False

getAnswer :: Int -> String -> Int
getAnswer n xs = sum $ map ((\xs' -> (10 * head xs' + last xs')) . map (\x -> ord x - 48) . toDigit n) (getResult $ parse sepLines xs)

main = do inp <- getInput
          print ("Part 1: " ++ show (getAnswer 1 inp))
          print ("Part 2: " ++ show (getAnswer 2 inp))