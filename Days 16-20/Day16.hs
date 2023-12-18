getInput = do inp <- readFile "Day16Input.txt"
              return (lines inp)

main = do inp <- getInput
          putStrLn (unlines inp)
          print (part1 inp)
          let res = (part2 inp)
          print (res)
          print (maximum [y | (x,y) <- res])
      
part1 :: [String] -> Int
part1 inp = fst (getAns inp ((0,0),'R') [])

getAns :: [String] -> ((Int,Int),Char) -> [((Int,Int),Char)] -> (Int,[((Int,Int),Char)])
getAns inp s prev = (sum [if (i,j) `elem` res then 1 else 0 | j <- [0..(length inp - 1)], i <- [0..(length (head inp) - 1)]],coords)
    where res = map (\((a,b),c) -> (a,b)) coords
          coords = travel inp (length (head inp), length inp) [s] [s] prev

travel :: [String] -> (Int,Int) -> [((Int,Int),Char)] -> [((Int,Int),Char)] -> [((Int,Int),Char)] -> [((Int,Int),Char)]
travel _ _ [] prev history = prev
travel map dim@(w,h) xs prev history = let new = concat [[x' | x'@((a,b),c) <- move x, c /= 'n', x' `notElem` prev, not (a < 0 || a >= w || b < 0 || b >= h )] | x <- xs]
                                        in travel map dim new (new ++ prev) history
    where 
        move :: ((Int,Int),Char) -> [((Int,Int),Char)]
        move c@(p@(i,j),d) | i < 0 || i >= w || j < 0 || j >= h = [((0,0),'n')]
                           | x == '.' || (x == '-' && (d == 'L' || d == 'R')) || (x == '|' && (d == 'U' || d == 'D')) = [(addV p newPos,d)]
                           | x == '-' && (d == 'U' || d == 'D') = [((i-1,j),'L'), ((i+1,j),'R')]
                           | x == '|' && (d == 'L' || d == 'R') = [((i,j-1),'U'), ((i,j+1),'D')]
                           | x == '/'  = [if d == 'L' then ((i,j+1),'D') else if d == 'R' then ((i,j-1),'U') else if d == 'U' then ((i+1,j),'R') else ((i-1,j),'L')]
                           | x == '\\' = [if d == 'L' then ((i,j-1),'U') else if d == 'R' then ((i,j+1),'D') else if d == 'U' then ((i-1,j),'L') else ((i+1,j),'R')]
            where 
                addV (a,b) (c,d) = (a+c,b+d)
                x = (map !! j) !! i
                newPos | d == 'L' = (-1,0)
                       | d == 'R' = (1,0)
                       | d == 'U' = (0,-1)
                       | d == 'D' = (0,1)

part2 :: [String] -> [(Int,Int)]
part2 inp = tryAllEdges 0 inp [] ts
    where
        tryAllEdges :: Int -> [String] -> [((Int,Int),Char)] -> [(Int,Int)] -> [(Int,Int)]
        tryAllEdges n _ _ [] = []
        tryAllEdges n map prev (x:xs) | j < length map = (n,fst res) : tryAllEdges (n+1) map (snd res) xs
                                      | otherwise = []
            where
                res = getAns map (x,d) prev
                (i,j) = x

                d | j == 0 = 'D'
                  | j == length map - 1 = 'U'
                  | i == 0 = 'R'
                  | otherwise = 'L'

        ts = [(i,0) | i <- [0..(length (head inp) - 1)]] ++ [(i,length inp - 1) | i <- [0..(length (head inp) - 1)]]
             ++ [(0,j) | j <- [1..(length (head inp) - 2)]] ++ [(length (head inp),j) | j <- [1..(length (head inp) - 2)]]

