getInput = do inp <- readFile "Day19Input.txt"
              return (map lines [takeWhile (/='-') inp, drop 2 (dropWhile (/='-') inp)])


type Rule = (String,(Int,Int,Int,Int) -> String)


r1 :: Rule
r1 = ("px",\(x,m,a,s) -> if a < 2006 then "qkq" else if m > 2090 then "A" else "rfg")

applyRule :: Rule -> (Int,Int,Int,Int) -> String
applyRule (label, f) = f

splitString :: Char -> String -> [String]
splitString _ [] = []
splitString c xs = let n = takeWhile (/=c) xs
                    in filter (not . null) (n : (splitString c .drop 1 . dropWhile (/=c)) xs)


genParts :: [String] -> [(Int, Int, Int, Int)]
genParts [] = []
genParts (x:xs) = let x' = map (read . drop 2) (splitString ',' (take (length x - 2) (tail x)) ) :: [Int]
                   in (head x', x' !! 1, x' !! 2, last x') : genParts xs


genRules :: [[String]] -> [Rule]
genRules [] = []
genRules (x:xs) = rule : genRules xs
    where
        rule = (label, func rules)
        label = head x
        rules = tail x

        func :: [String] -> ((Int,Int,Int,Int) -> String)
        func (x':xs) | ':' `elem` x' =  if h == 'x' then (\t@(x,m,a,s) -> if x `oper` val then newLabel else func xs t) else
                                        if h == 'm' then (\t@(x,m,a,s) -> if m `oper` val then newLabel else func xs t) else
                                        if h == 'a' then (\t@(x,m,a,s) -> if a `oper` val then newLabel else func xs t) else
                                                         (\t@(x,m,a,s) -> if s `oper` val then newLabel else func xs t)
                     | otherwise = const x'
            where h = head x'
                  op = x' !! 1
                  val | ':' `elem` x' = read (takeWhile (/=':') (drop 2 x'))
                      | otherwise = 0

                  oper | op == '>' = (>)
                       | op == '<' = (<)

                  newLabel = last (splitString ':' x')

applyRules :: (Int,Int,Int,Int) -> [Rule] -> String -> String
applyRules x rs lab = let res = head [f | (l,f) <- rs, l == lab] x
                       in if res `elem` ["A","R"] then res else applyRules x rs res

part1 :: [[String]] -> Int
part1 xss = sum (zipWith (curry (\((x,m,a,s),r) -> if r == "A" then x + m + a + s else 0)) parts ([applyRules p rules "in" | p <- parts]))
    where
        rules = genRules (map formatRule (head xss))
        parts = genParts (last xss)

part2 :: [[String]] -> Int
part2 xss = let poss = filter (=="A") [applyRules x rules "in" | x <- gens]
             in length poss
    where
        rules = genRules (map formatRule (head xss))
        gens =[(x,m,a,s) | x <- [1000,2000,3000,4000], m <- [1000,2000,3000,4000], a <- [1000,2000,3000,4000], s <- [1000,2000,3000,4000]]


main = do inp <- getInput
          print (head inp)
          print (last inp)
          print [(formatRule x)| x <- head inp]
          print [filterRules 'm' (formatRule x)| x <- head inp]
          print [acceptedRanges (zip [1..4000] [applyRules (if c == 'x' then (x,0,0,0) else if c == 'm' then (0,x,0,0) else if c == 'a' then (0,0,x,0) else (0,0,0,x)) (genRules (map (filterRules c . formatRule) (head inp))) "in" | x <- [1..4000]]) | c <- ['x','m','a','s']]

type RuleRange = [(Char, (String, [(String,(Int,Int))]))]

formatRule :: String -> [String]
formatRule x = label : rules
    where
        label = takeWhile (/='{') x
        rules = (map (filter (/='}')) .splitString ',' . drop 1 . dropWhile (/='{')) x

filterRules :: Char -> [String] -> [String]
filterRules c = filter (\x -> notElem ':' x || (head x == c))

acceptedRanges :: [(Int,String)] -> [(Int,Int)]
acceptedRanges [] = []
acceptedRanges xs = let ac = takeWhile (\x -> snd x == "A") xs
                     in ([(fst (head ac), fst (last ac)) | (not . null) ac]) ++ acceptedRanges (drop 1 (dropWhile (\x -> snd x == "A") xs))