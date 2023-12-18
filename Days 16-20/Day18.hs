getInput = do inp <- readFile "Day18Input.txt"
              return (map words (lines inp))

main = do inp <- getInput
          putStrLn ("Part 1: " ++ show (part1 inp))
          putStrLn ("Part 2: " ++ show (part2 inp))

genShape :: [[String]] -> (Int,Int) -> [((Int,Int),(Int,Int))]
genShape [] _ = []
genShape ((d:n:_):xs) (i,j) = ((i,j),new) : genShape xs new
    where
        new | d == "R" = (i + read n,j)
            | d == "L" = (i - read n,j)
            | d == "U" = (i,j - read n)
            | d == "D" = (i,j + read n)
    
borderVol :: [((Int,Int),(Int,Int))] -> Int
borderVol [] = 0
borderVol ((c1,c2):ls) | fst c1 == fst c2 = abs (snd c1 - snd c2) + borderVol ls
                       | snd c1 == snd c2 = abs (fst c1 - fst c2) + borderVol ls

shoelace :: [(Int,Int)] -> (Int,Int) -> Int
shoelace [c1] c2 = (fst c1 * snd c2) - (snd c1 * fst c2)
shoelace (c1:c2:ls) f = (fst c1 * snd c2) - (snd c1 * fst c2) + shoelace (c2:ls) f

part1 :: [[String]] -> Int
part1 inp = let res = genShape inp (0,0)
                interior = shoelace (map fst res) (fst (head res)) `div` 2
                exterior = borderVol res
             in interior + (exterior `div` 2) + 1

hexToDec :: Int -> String -> Int
hexToDec _ [] = 0
hexToDec n (x:xs) = (16^n) * mapHex x + hexToDec (n-1) xs
    where
        vals :: [(Char,Int)]
        vals = zip ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'] [0..15]
        mapHex :: Char -> Int
        mapHex n = head [y | (x,y) <- vals, x == n]

decode :: [[String]] -> [[String]]
decode [] = []
decode (x:xs) = let hex = drop 2 (last x)
                 in [[d], (show . hexToDec 5 . take 5) hex] : decode xs
    where d = head [m | (n,m) <- zip ['0','1','2','3'] ['R','D','L','U'], n == drop 2 (last x) !! 5]

part2 :: [[String]] -> Int
part2 xss = part1 (decode xss)