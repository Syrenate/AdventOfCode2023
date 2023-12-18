import Data.List

getInput :: IO [String]
getInput = do inp <- readFile "Day14Input.txt"
              return (lines inp)

main = do inp <- getInput
          print (part1 inp)
          let ans = part2 inp []
          let cyc = drop (snd (snd ans)) (fst ans)
          let (m,n) = snd ans
          putStrLn (concat [unlines x ++ "\n" | x <- fst ans])
          print (snd ans)
          print ([northLoad x | x <- fst ans]) -- !! ((1000000000 - snd (snd ans)) `mod` ((fst (snd ans) - snd (snd ans)) - 1)))
          print (northLoad (fst ans !! (m - (getVal n (m-n) - 1000000000) - 1)))
          print (getVal n (m-n))
    where
        getVal :: Int -> Int -> Int
        getVal x step = x + step * round (1000000000 / fromIntegral step)

tiltNorth :: [String] -> (Int,Int) -> [String]
tiltNorth g (i,j) | i == 0 && j == height + 1 = g
                  | otherwise = tiltNorth (if (g !! j) !! i == 'O' then moveUp g (i,j) else g) (if i == width then (0,j+1) else (i+1,j))
    where width = length (head g) - 1
          height = length g - 1

moveUp :: [String] -> (Int,Int) -> [String]
moveUp xss (i,j) = replaceItem xss (i,j) (i, new j)
    where
        new :: Int -> Int
        new x | x == 0 = 0
              | (xss !! (x-1)) !! i == '.' = new (x-1)
              | otherwise = x

replaceItem :: [String] -> (Int,Int) -> (Int,Int) -> [String]
replaceItem xss (i,j) (i',j') = take j' new ++ [take i' (new !! j') ++ "O" ++ drop (i'+1) (new !! j')] ++ drop (j'+1) new
    where new = take j xss ++ [take i (xss !! j) ++ "." ++ drop (i+1) (xss !! j)] ++ drop (j+1) xss

northLoad :: [String] -> Int
northLoad xss = sum [sum [if x == 'O' then length xss - j else 0 | x <- xs] | (xs,j) <- zip xss [0..]]

part1 :: [String] -> Int
part1 xs = northLoad (tiltNorth xs (0,0))

cycleL :: [String] -> [String]
cycleL xs = cont (\x -> rotC (tiltNorth x (0,0))) xs 4

rotC :: [[a]] -> [[a]]
rotC = map reverse . transpose

cont :: ([a] -> [a]) -> [a] -> Int -> [a]
cont f xs n | n /= 0 = cont f (f xs) (n-1)
            | otherwise = xs

part2 :: [String] -> [[String]] -> ([[String]],(Int,Int))
part2 xs xss | c `elem` xss = (xss ++ [c],(length xss + 1, head [i | (x,i) <- zip xss [0..], x == c] + 1))
             | otherwise = part2 c (xss ++ [c])
    where c = cycleL xs