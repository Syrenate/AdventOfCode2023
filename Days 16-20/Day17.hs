{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use list comprehension" #-}
getInput = do inp <- readFile "Day17Input.txt"
              return [map (\x -> fromEnum x - 48) line | line <- lines inp]

main = do inp <- getInput
          putStrLn (unlines (map show inp))
          let (i,j) = (length (head inp) - 1, length inp - 1)
          print (dijkstras inp [] (mapToNodes inp) initial 0)

type Node = ((Int,Int), Int) -- Coord, distance to initial node

initial = ((0,0),0) :: Node

mapToNodes :: [[Int]] -> [Node]
mapToNodes xss = concat [[if (i,j) == (0,0) then ((0,0),0) else ((i,j),999999999999) | (x,i) <- zip xs [0..]] | (xs,j) <- zip xss [0..]]

dijkstras grid visited unvisited curr n | null unvisited || fst new == target = [new]
                                        | otherwise = new : dijkstras grid (new : visited) nextUnv new (n+1)
    where
        sorted = quicksort1 (filter (\x -> snd x /= 999999999999) unvisited)
        new = if null sorted then curr else head sorted
        unv' = tail sorted

        (i,j) = fst new
        nextTo = [(i+1,j),(i-1,j),(i,j+1),(i,j-1)]
        neighbours = [n | n@(c,v) <- unv', c `elem` nextTo]

        nextUnv = [if c `elem` map fst neighbours then let alt = snd new + (grid !! snd c) !! fst c in if alt < v then (c,alt) else n else n | n@(c,v) <- unv']
        target = (length (head grid) - 1, length grid - 1)


quicksort1 :: [Node] -> [Node]
quicksort1 [] = []
quicksort1 (x:xs) =
    let smallerSorted = quicksort1 [a | a <- xs, snd a <= snd x]
        biggerSorted = quicksort1 [a | a <- xs, snd a > snd x]
    in  smallerSorted ++ [x] ++ biggerSorted