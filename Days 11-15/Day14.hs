import Data.List

getInput :: IO [String]
getInput = do inp <- readFile "Day14Input.txt"
              return (lines inp)

main = do inp <- getInput
          print (part1 (tiltNorth inp))
          let xs = part2 inp
          print (snd xs)
          print (part1 ((fst ((reverse (fst xs)) !! 85))))
          print (snd (part2 (fst (head (fst xs)))))
          print (part1 (fst (fst xs !! (17 - ((1000000000 - snd (snd xs)) `mod` (fst (snd xs) - snd (snd xs)))))))

part2 :: [String] -> ([([String],Int)],(Int,Int))
part2 css = go [] css 0
    where
        go :: [([String],Int)] -> [String] -> Int -> ([([String],Int)],(Int,Int))
        go col xs n = let new = rotateC (tiltNorth (rotateC (tiltNorth (rotateC (tiltNorth (rotateC (tiltNorth xs)))))))
                       in if or [new == fst c | c <- col] then ((new,n) : col, (n,head [i | (x,i) <- col, x == new])) else go ((new, n) : col) new (n+1)

tiltNorth :: [String] -> [String]
tiltNorth inp = let rocks = concat [[(i,j) | (c,i) <- zip cs [0..], c == 'O'] | (cs,j) <- zip inp [0..]]
                 in moveRocks rocks inp
    where
        moveRocks :: [(Int,Int)] -> [String] -> [String]
        moveRocks [] css = css
        moveRocks (r:rs) css = moveRocks rs (moveUp css r max r)

        max = length (head inp)

rotateC :: [String] -> [String]
rotateC = transpose . reverse

moveUp :: [String] -> (Int,Int) -> Int -> (Int,Int) -> [String]
moveUp css (ii,ji) width (i,j) | j - 1 < 0 || (css !! (j-1)) !! i /= '.' = replaceEntry css (ii,ji) (i,j)
                               | otherwise = moveUp css (ii,ji) width (i,j-1)

replaceEntry :: [String] -> (Int,Int) -> (Int,Int) -> [String]
replaceEntry cs (i,j) (i',j') = take j' css ++ [take i' (css !! j') ++ "O" ++ drop (i'+1) (css !! j')] ++ drop (j'+1) css
    where css = take j cs ++ [take i (cs !! j) ++ "." ++ drop (i+1) (cs !! j)] ++ drop (j+1) cs


displayGrid :: [String] -> String
displayGrid css = concat [c ++ "\n" | c <- css]

part1 :: [String] -> Int
part1 css = sum [length (filter (=='O') cs) * (length css + 1 - i) | (cs,i) <- zip css [1..]]