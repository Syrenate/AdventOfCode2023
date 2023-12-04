import MyParser

getInput :: IO [(Int,[Int],[Int])]
getInput = do input <- readFile "Day4Input.txt"
              return [getResult (parse parseInput4 line) | line <- getResult (parse sepLines input)]


getScore :: (Int,[Int],[Int]) -> Int
getScore (_,xs,ys) = let s = sum[1 | x <- xs, x `elem` ys]
                      in if s == 0 then 0 else 2^(s-1)


part1 :: [(Int,[Int],[Int])] -> Int
part1 xs = sum [getScore x | x <- xs]

part2 :: [(Int,[Int],[Int])] -> Int
part2 inp = let results = [sum[1 | x <- xs, x `elem` ys] | (x,xs,ys) <- inp]
             in sum [y | (x,y) <- go [(i,1) | i <- [1..(length results)]] results 0]
    where
        go :: [(Int,Int)] -> [Int] -> Int -> [(Int,Int)] -- [(Card number, # of instances)]
        go is [] _ = is
        go is@((x,y):xs) nss@(n:ns) r = go (editInstances is n (is !! r)) ns (r+1)

        editInstances :: [(Int,Int)] -> Int -> (Int,Int) -> [(Int,Int)]
        editInstances ns r (x,y) =  [if n1 `elem` [(x+1)..(x+r)] then (n1,n2+y) else n | n@(n1,n2) <- ns] 

main :: IO ()
main = do input <- getInput
          print (part1 input)
          print (part2 input)