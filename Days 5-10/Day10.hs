import Data.List

getInput :: IO [String]
getInput = do xs <- readFile "Day10Input.txt"
              return (lines xs)

parseCoords :: [[Char]] -> [Coord]
parseCoords xss = concat [[makeCoord ((i,j),x) | (x,i) <- zip xs [0..]] | (xs,j) <- zip xss [0..]]

move :: ((Int,Int),Char) -> [(Int,Int)]
move ((x,y),c) | c == '|' = [(x,y-1),(x,y+1)]
               | c == '-' = [(x-1,y),(x+1,y)]
               | c == 'L' = [(x+1,y),(x,y-1)]
               | c == 'J' = [(x-1,y),(x,y-1)]
               | c == '7' = [(x-1,y),(x,y+1)]
               | c == 'F' = [(x+1,y),(x,y+1)]
               | c == 'S' = [(x+1,y),(x-1,y),(x,y-1),(x,y+1)]
               | otherwise = []

type Coord = ((Int, Int), Char)

makeCoord :: ((Int,Int),Char) -> Coord
makeCoord x = x

pos :: Coord -> (Int,Int)
pos ((x,y),_) = (x,y)

val :: Coord -> Char
val (_,c) = c

validCons :: [(Char, [(Int,Int)])]
validCons = [('|', [(0,1),(0,-1)]),
             ('-', [(1,0),(-1,0)]),
             ('L', [(1,0),(0,-1)]),
             ('J', [(-1,0),(0,-1)]),
             ('7', [(-1,0),(0,1)]),
             ('F', [(1,0),(0,1)])]

getConns :: Coord -> [Coord] -> [Coord]
getConns st@((x,y),c) map | c == 'S' = [makeCoord ((x+i,y+j), getVal (x+i,y+j) map) | (i,j) <- [(1,0),(0,1),(-1,0),(0,-1)], x+i >= 0, y+j >= 0, st `elem` getConns (makeCoord ((x+i,y+j), getVal (x+i,y+j) map)) map]
                          | c /= 'S' = concat [[makeCoord ((x+i,y+j), getVal (x+i,y+j) map) | (i,j) <- cs] | (c',cs) <- validCons, c == c']
    where
        getVal :: (Int,Int) -> [Coord] -> Char
        getVal (i,j) cs = head [val cord | cord <- cs, pos cord == (i,j)]

startTravel :: [Coord] -> [Coord]
startTravel map = let st = head [c | c <- map, val c == 'S']
                   in travel [st] st (head (getConns st map)) map
    where
        travel :: [Coord] -> Coord -> Coord -> [Coord] -> [Coord]
        travel ns prev curr map | val curr == 'S' = ns
                                | otherwise = travel (curr : ns) curr (head [c | c <- getConns curr map, c /= prev]) map

part1 :: [Coord] -> Int
part1 inp = length inp `div` 2

main :: IO ()
main = do inp <- getInput
          let xs = startTravel (parseCoords inp)
          print (part1 xs)
          print (part2 (parseCoords inp) xs)

replaceMap :: Int -> [Coord] -> [Coord] -> String
replaceMap _ [] _ = []
replaceMap n (c:cs) loop | fst (pos c) == n - 1 = (if val c == 'S' then 'J' else (if c `elem` loop then val c else '.')) : "\n" ++ replaceMap n cs loop
                           | otherwise          = (if c `elem` loop then val c else '.') : replaceMap n cs loop

part2 :: [Coord] -> [Coord] -> Int
part2 xs loop = getEnclosed (replaceMap (length (head xs)) xs loop)

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