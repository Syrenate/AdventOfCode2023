module MyParser where

import Control.Applicative ( Alternative(some, empty, (<|>)) )
import Data.Char

newtype Parser a = P (String -> [(a,String)])

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (const [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

item :: Parser Char -- Parses first char of string
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

parse :: Parser a -> String -> [(a,String)] -- Applies parser to string
parse (P p) = p

getResult :: [(a,String)] -> a
getResult ((x,xs):xss) = x

getResultM :: [(a,String)] -> Maybe a
getResultM [] = Nothing
getResultM ((x,xs):xss) = Just x

choice :: Parser a -> Parser a -> Parser a
choice p q = p <|> q

loop :: Parser a -> Parser [a]
loop p = some p

-- Primative Parsers

satisfy :: (Char -> Bool) -> Parser Char -- Parses first char of string given it satisfies a given predicate
satisfy p = do x <- item;
               if p x then return x else empty

digit :: Parser Char
digit = do x <- item
           if isDigit x then return x else empty

notDigit :: Parser Char
notDigit = do x <- item
              if not (isDigit x) then return x else empty

char :: Char -> Parser Char
char a = do x <- item
            if x == a then return a else empty

nat :: Parser Int
nat = do xs <- some digit;
         return (read xs)

int :: Parser Int
int = do char '-';
         nat
       <|>
        nat

-- Sequence Parsers

sepLines :: Parser [String]
sepLines = some temp
    where
        temp :: Parser String
        temp = do x <- some (satisfy (`notElem` "\n"));
                  string "\n";
                  return x;
                  <|>
                  some (satisfy (`notElem` "\n"))

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)