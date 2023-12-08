getInput = do inp <- readFile "Day6Input.txt"; 
              return (zip (map read (take 4 (drop 1 (words inp)))) (map read (drop 6 (words inp)))) :: IO [(Integer,Integer)]

getRange (t,x) = let disc = sqrt (fromInteger t ^ 2 - 4 * fromInteger x) :: Float
                  in floor (((fromInteger t + disc) / 2) - 0.0001) - ceiling (((fromInteger t - disc) / 2) + 0.0001) + 1 :: Integer

main = do inp <- getInput; 
          print (product [getRange x | x <- inp]); 
          print ((getRange . correctAnswer) inp)
    where
        correctAnswer inp = (read (concat [show x | (x,y) <- inp]), read (concat [show y | (x,y) <- inp]))