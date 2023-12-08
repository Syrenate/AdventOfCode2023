import MyParser -- Custom Parsing Library
import Data.Char

toDigit :: Int -> String -> String -- Takes input string, outputs list of contained digits (n==1 => part 1, n==2 => part 2)
toDigit _ [] = []
toDigit n s | or [[n] `isPrefixOf` s | n <- ['1','2','3','4','5','6','7','8','9']] = head s : toDigit n (tail s) -- For digits
			| n==2 && "one" `isPrefixOf` s   = '1' : toDigit n (drop 2 s)                                        -- For names of digits
			| n==2 && "two" `isPrefixOf` s   = '2' : toDigit n (drop 2 s)
			| n==2 && "three" `isPrefixOf` s = '3' : toDigit n (drop 4 s)
			| n==2 && "four" `isPrefixOf` s  = '4' : toDigit n (drop 3 s)
			| n==2 && "five" `isPrefixOf` s  = '5' : toDigit n (drop 3 s)
			| n==2 && "six" `isPrefixOf` s   = '6' : toDigit n (drop 2 s)
			| n==2 && "seven" `isPrefixOf` s = '7' : toDigit n (drop 4 s)
			| n==2 && "eight" `isPrefixOf` s = '8' : toDigit n (drop 4 s)
			| n==2 && "nine" `isPrefixOf` s  = '9' : toDigit n (drop 3 s)
			| otherwise = toDigit n $ tail s                                                                     -- For anything else
	where
		isPrefixOf :: String -> String -> Bool -- Function for evaluating if a given string is the prefix of another given string
		isPrefixOf xs' xs | length xs < length xs'      = False
						  | take (length xs') xs == xs' = True
						  | otherwise                   = False

getAnswer :: Int -> String -> Int -- Evaluates the sum of all calibration values in an input, xs, after parsing seperate lines of xs.
getAnswer n xs = sum $ map ((\xs' -> (10 * head xs' + last xs')) . map (\x -> ord x - 48) . toDigit n) (getResult $ parse sepLines xs)

main = do inp <- readFile "Day1Input.txt" -- Display Answers
          print ("Part 1: " ++ show (getAnswer 1 inp))
          print ("Part 2: " ++ show (getAnswer 2 inp))