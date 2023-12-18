import Data.Char

getInput = do inp <- readFile "Day15Input.txt"
              return (words (map (\x -> if x == ',' then ' ' else x) inp))

main = do inp <- getInput
          print (part1 inp)
          print (part2 inp)

hashString :: Int -> String -> Int
hashString = foldl (\ n x -> (17 * (n + ord x)) `mod` 256)

part1 :: [String] -> Int
part1 xs = sum [hashString 0 x | x <- xs]


newtype Hashmap = H [([(String,Int)],Int)]
def = H [([],i) | i <- [0..255]] :: Hashmap

hashInput :: [String] -> Hashmap -> Hashmap
hashInput [] h = h
hashInput f@(x:xs) (H hs) | '=' `elem` x = let y = [takeWhile isAlpha x, [last x]]
                                               hash = hashString 0 (head y)
                                            in hashInput xs (H [if n == hash then (if null [n' | n' <- ns, fst n' == head y]
                                                                                 then (head y, read (last y)) : ns
                                                                                 else [if fst n' == head y then (head y, read (last y)) else n' | n' <- ns],n) else (ns,n) | (ns,n) <- hs])
                          | '-' `elem` x = let y = takeWhile isAlpha x
                                               hash = hashString 0 y
                                            in hashInput xs (H [if n == hash then (filter (\x -> fst x /= y) ns,n) else (ns,n) | (ns,n) <- hs])

focusingPower :: Hashmap -> Int
focusingPower (H []) = 0
focusingPower (H h@((ns,n):hs)) = sum [(n + 1) * i * num | ((str,num),i) <- zip (reverse ns) [1..]] + focusingPower (H hs)

part2 :: [String] -> Int
part2 xs = focusingPower (hashInput xs def)