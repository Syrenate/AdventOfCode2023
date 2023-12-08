import Data.List

getInput :: IO [String]
getInput = do inp <- readFile "Day7Input.txt"
              return (words inp)

getType :: Int -> String -> Int
getType p str | or [i == 5 | (x,i) <- getInst p str] = 7
              | or [i == 4 | (x,i) <- getInst p str] = 6
              | or [i == 3 | (x,i) <- getInst p str] = if or [i == 2 | (x,i) <- getInst p str] then 5 else 4
              | sum [if i == 2 then 1 else 0 | (x,i) <- getInst p str] == 2 = 3
              | or [i == 2 | (x,i) <- getInst p str] = 2
              | otherwise = 1

getInst :: Int -> String -> [(Char,Int)]
getInst p inp = let ns = go inp (removeDupes inp)
                 in if p == 2 then map (\(c,i) -> if c /= fst (maxim (' ',0) ns) then (c,i - dupes p inp 'J') else (c,i)) ns else ns
    where
        go :: String -> String -> [(Char,Int)]
        go ys [] = []
        go ys (x:xs) = (x, modfy x ys) : go ys xs

        modfy :: Char -> String -> Int
        modfy _ [] = 0
        modfy n (x:xs) | n == x || (p == 2 && x == 'J') = 1 + modfy n xs
                       | otherwise = modfy n xs

        maxim :: (Char,Int) -> [(Char,Int)] -> (Char,Int)
        maxim (c,i) [] = (c,i)
        maxim (c',i') ((c,i):xs) | i > i' = maxim (c,i) xs
                                 | otherwise = maxim (c',i') xs

        removeDupes :: Eq a => [a] -> [a]
        removeDupes [] = []
        removeDupes (x:xs) | x `elem` xs = removeDupes xs
                           | otherwise   = x : removeDupes xs


isGreater :: Int -> String -> String -> Bool
isGreater p xs' ys' = go [getVal p x | x <- xs'] [getVal p x | x <- ys']
    where
        go :: [Int] -> [Int] -> Bool
        go (x:xs) (y:ys) | x == y = go xs ys
                         | x /= y = x > y
                         
        cards  = ['A','K','Q','J','T','9','8','7','6','5','4','3','2']
        getVal :: Int -> Char -> Int
        getVal p x = if p == 1 || (p == 2 && x /= 'J') then head [20 - i | (c,i) <- zip cards [0..], c == x] else 0

dupes :: Int -> [Char] -> Char -> Int
dupes _ [] _ = 0
dupes p xs n = length (filter (\x -> x == n || (p == 2 && x == 'J')) xs)



getAnswer :: Int -> [String] -> Int
getAnswer p xs = sum [i * getBid x xs | (x,i) <- getRank p (organise p [x | (x,i) <- zip xs [1..], odd i])]
    where
        organise :: Int -> [String] -> [(Int,[String])]
        organise _ [] = [(x,[]) | x <- [1..7]]
        organise p (x:xs) = x `addTo` organise p xs
            where
                addTo :: String -> [(Int,[String])] -> [(Int,[String])]
                addTo n struc = [if getType p n == i then (i,n:hs) else (i,hs) | (i,hs) <- struc]

        getRank :: Int -> [(Int,[String])] -> [(String,Int)]
        getRank p xs = zip (compile p xs) [1..]

        compile :: Int -> [(Int,[String])] -> [String]
        compile _ [] = []
        compile p ((_,xs):xss) = quicksort p xs ++ compile p xss
            where
                quicksort :: Int -> [String] -> [String]
                quicksort _ [] = []
                quicksort p (x:xs) =
                    let lt = quicksort p [a | a <- xs, not (isGreater p a x)]
                        gt = quicksort p [a | a <- xs, isGreater p a x]
                     in lt ++ [x] ++ gt

        getBid :: String -> [String] -> Int
        getBid n (x:xs) | x == n = read (head xs)
                        | otherwise = getBid n (tail xs)

part1 :: [String] -> Int
part1 = getAnswer 1

part2 :: [String] -> Int
part2 = getAnswer 2

main :: IO ()
main = do inp <- getInput
          print (part1 inp)
          print (part2 inp)