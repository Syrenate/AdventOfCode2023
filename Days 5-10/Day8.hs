import Data.List

getInput :: IO (String, [(String,(String,String))])
getInput = do input <- readFile "Day8Input.txt"
              return ((head . words) input,(parse . tail . words) input)
    where
        parse :: [String] -> [(String,(String,String))]
        parse [] = []
        parse inp = let xs = take 4 inp
                     in (head xs, (take 3 (drop 1 (xs !! 2)), take 3 (xs !! 3))) : parse (drop 4 inp)

trav1 :: String -> String -> [(String,(String,String))] -> Int -> String -> Int
trav1 ds' [] xss n ps = trav1 ds' ds' xss n ps 
trav1 ds' (d:ds) xss n ps | ps == "ZZZ" = n
                          | otherwise     = trav1 ds' ds xss (n+1) (if d == 'L' then fst (snd (quicksearch ps xss)) else snd (snd (quicksearch ps xss)))


trav :: String -> String -> [(String,(String,String))] -> Int -> String -> Int
trav ds' [] xss n ps = trav ds' ds' xss n ps 
trav ds' (d:ds) xss n ps | "Z" `isSuffixOf` ps = n
                         | otherwise     = trav ds' ds xss (n+1) (if d == 'L' then fst (snd (quicksearch ps xss)) else snd (snd (quicksearch ps xss)))

quicksort :: [(String,(String,String))] -> [(String,(String,String))]
quicksort [] = []
quicksort (s@(lab,(l,r)):xss) = 
    let
        lt = quicksort [b | b@(a,(l',r')) <- xss, a < lab]
        gt = quicksort [b | b@(a,(l',r')) <- xss, a >= lab]
    in
        lt ++ [s] ++ gt

quicksearch :: String -> [(String,(String,String))]-> (String,(String,String))
quicksearch x xss | x == fst rslt = rslt
                    | x >= fst rslt = quicksearch x (drop (length xss `div` 2) xss)
                    | x <  fst rslt = quicksearch x (take (length xss `div` 2) xss)
    where
        rslt = xss !! (length xss `div` 2)

lcmOfList :: [Int] -> Int -> Int
lcmOfList xs n | n == length xs - 1 = xs !! n
               | otherwise = 
                        let a = xs !! n
                            b = lcmOfList xs (n+1)
                         in (a*b) `div` gcd a b
                      
part1 :: (String, [(String,(String,String))]) -> Int
part1 inp = trav1 (fst inp) (fst inp) ((quicksort . snd) inp) 0 "AAA"

part2 :: (String, [(String,(String,String))]) -> Int
part2 inp = lcmOfList [trav (fst inp) (fst inp) (quicksort (snd inp)) 0 lab | (lab,(l,r)) <- snd inp, "A" `isSuffixOf` lab] 0

main :: IO ()
main = do inp <- getInput
          print (part1 inp)
          print (part2 inp)