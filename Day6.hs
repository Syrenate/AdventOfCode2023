getInput :: IO [(Integer,Integer)]
getInput = do inp <- readFile "Day6Input.txt"
              return (zip (map read (take 3 (drop 1 (words inp)))) (map read (drop 5 (words inp))))

evalMargin :: (Integer,Integer) -> Integer
evalMargin (t,x) = sum [if x < (t-t')*t' then 1 else 0 | t' <- [1..t]]

part1 :: [(Integer,Integer)] -> Integer
part1 xs = product [evalMargin x | x <- xs]

getLowerBound :: (Integer,Integer) -> Integer
getLowerBound (t,x) = go 10000 0
    where
        go :: Integer -> Integer -> Integer
        go step n | x < (t-n)*n = if step == 1 then n else go (step `div` 100) (n-step)
                  | otherwise = go step (n+step)

getUpperBound :: (Integer,Integer) -> Integer
getUpperBound (t,x) = go 10000 t
    where
        go :: Integer -> Integer -> Integer
        go step n | x < (t-n)*n = if step == 1 then n else go (step `div` 10) (n+step)
                  | otherwise = go step (n-step)

part2 :: (Integer,Integer) -> Integer
part2 s@(t,x) = getUpperBound s - getLowerBound s + 1


main :: IO ()
main = do inp <- getInput
          print (part1 inp)
          print (part2 (read (concat [show x | (x,y) <- inp]), read (concat [show y | (x,y) <- inp])))