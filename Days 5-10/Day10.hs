import Data.List

type Coord = ((Int, Int), Char) -- Easier to work with

pos :: Coord -> (Int,Int)
pos ((x,y),_) = (x,y)

val :: Coord -> Char
val (_,c) = c


getInput :: IO [String]
getInput = do xs <- readFile "Day10Input.txt"
              return (lines xs)

parseCoords :: [[Char]] -> [Coord] 
parseCoords xss = concat [[((i,j),x) | (x,i) <- zip xs [0..]] | (xs,j) <- zip xss [0..]]

validCons :: [(Char, [(Int,Int)])] -- Finds the next possible coords from a given character
validCons = [('|', [(0,1),(0,-1)]), ('-', [(1,0),(-1,0)]),
             ('L', [(1,0),(0,-1)]), ('J', [(-1,0),(0,-1)]),
             ('7', [(-1,0),(0,1)]), ('F', [(1,0),(0,1)])]

-- Part 1 Funcs --
getConns :: Coord -> [Coord] -> [Coord] -- Gets valid connections from any given coordinate
getConns st@((x,y),c) map | c == 'S' = [((x+i,y+j), getVal (x+i,y+j) map) | (i,j) <- [(1,0),(0,1),(-1,0),(0,-1)], x+i >= 0, y+j >= 0, st `elem` getConns ((x+i,y+j), getVal (x+i,y+j) map) map]
                          | c /= 'S' = concat [[((x+i,y+j), getVal (x+i,y+j) map) | (i,j) <- cs] | (c',cs) <- validCons, c == c']
    where
        getVal :: (Int,Int) -> [Coord] -> Char -- Gets the coordinate from a given x,y value.
        getVal (i,j) cs = head [val cord | cord <- cs, pos cord == (i,j)]

startTravel :: [Coord] -> [Coord] -- Traverses main loop starting from the 'S' coordinate
startTravel map = let st = head [c | c <- map, val c == 'S']
                   in travel [st] (head (getConns st map)) map
    where
        travel :: [Coord] -> Coord -> [Coord] -> [Coord] 
        travel ns curr map | val curr == 'S' = ns
                           | otherwise = travel (curr : ns) (head [c | c <- getConns curr map, c /= head ns]) map

part1 :: [Coord] -> Int
part1 inp = length inp `div` 2

-- Part 2 Funcs --
replaceMap :: Int -> [Coord] -> [Coord] -> String
replaceMap _ [] _ = []
replaceMap n (c:cs) loop | fst (pos c) == n - 1 = (if val c == 'S' then 'J' else (if c `elem` loop then val c else '.')) : "\n" ++ replaceMap n cs loop
                           | otherwise          = (if c `elem` loop then val c else '.') : replaceMap n cs loop

getEnclosed :: String -> Int
getEnclosed map = sum [evalLine 0 line | line <- lines map]
    where
        evalLine :: Int -> String -> Int
        evalLine i [] = 0
        evalLine i (c:cs) | c == '.' = (if odd i then 1 else 0) + evalLine i cs
                          | c == '-' = evalLine i cs
                          | c == 'F' = if head (dropWhile (=='-') cs) == 'J' then evalLine (i+1) (tail (dropWhile (=='-') cs)) else evalLine i (tail (dropWhile (=='-') cs))
                          | c == 'L' = if head (dropWhile (=='-') cs) == '7' then evalLine (i+1) (tail (dropWhile (=='-') cs)) else evalLine i (tail (dropWhile (=='-') cs))
                          | otherwise = evalLine (i+1) cs
                
part2 :: [Coord] -> [Coord] -> Int
part2 xs loop = getEnclosed (replaceMap (length (head xs)) xs loop)

-- Main Call
main :: IO ()
main = do inp <- getInput
          let xs = startTravel (parseCoords inp)
          print (part1 xs)
          print (part2 (parseCoords inp) xs)