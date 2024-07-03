import Data.Data
import Data.Char
import Data.List
--import Data.Text
import Data.Text.Internal.Read

unsafeLookup :: Eq a =>  a -> [(a,b)]-> b
unsafeLookup a []     = error "a kulcs nincs benne a listában"
unsafeLookup a (x:xs) = if (fst x == a) then snd x else unsafeLookup a xs


parseCharOfBase :: Integer -> Char -> Integer
parseCharOfBase base char = 
  if fromIntegral (hexDigitToInt char) < base 
  then fromIntegral (hexDigitToInt char) 
  else error "az adott karakter nem az adott számrendszer számjegye"
  
parseIntegerS :: Integer -> String -> (Integer, Integer)
parseIntegerS base text = Data.List.foldr (\x (result, h) -> (result + (parseCharOfBase base x * base^h), h+1)) (0, 0) text
  
parseInteger :: Integer -> String -> Integer
parseInteger base []   = error "üres a szöveg, nincs mit parsolni"
parseInteger base text = fst (parseIntegerS base text)


parseLiteralS :: Char -> String -> Integer
parseLiteralS c text
  | c `elem` ['b','B'] = parseInteger 2 text
  | c `elem` ['o','O'] = parseInteger 8 text
  | c `elem` ['x','X'] = parseInteger 16 text

parseLiteral :: String -> Integer --kell még a 2 elemnél kisebb esetek
parseLiteral (c1:c2:text) = 
  if c1 /= '0' 
  then parseInteger 10 (c1:c2:text) 
  else (parseLiteralS c2 text)
  


type Token = String

isOperator :: Char -> Bool
isOperator char = char `elem` ['(', ')', '+', '-', '~', '/', '%', '*']

delimeter :: Char
delimeter = 'd'

addDelimeter :: String -> String
addDelimeter [] = []
addDelimeter (x:xs) 
    | isOperator x = delimeter : x : delimeter : (addDelimeter xs)
    | otherwise = x : (addDelimeter xs)

split :: String -> [String]
split = foldr (\c (x:xs) -> if (c == delimeter) then []:(x:xs) else (c:x):xs ) [[]]

removeEmptyStrings :: [String] -> [String]
removeEmptyStrings xs = [ x | x <- xs , not (null x) ]

tokenize :: String -> [String]
tokenize [] = []
tokenize text = 
  removeEmptyStrings $ 
  split $ 
  addDelimeter (filter (\x -> (x/=' ')) text)



precedence :: Token -> Int
precedence token
  | token == "+" = 1
  | token == "-" = 1
  | token == "/" = 2
  | token == "*" = 2
  | token == "%" = 2
  | token == "~" = 3
  | otherwise    = error "nem operátor és/vagy nincs precedenciája"
  
  
  
takeUntilSmallerPrec :: Token -> [Token] -> [Token]
takeUntilSmallerPrec x [] = []
takeUntilSmallerPrec x (o:ops) = 
    if o == "(" || precedence o < precedence x 
    then []
    else o : (takeUntilSmallerPrec x ops)

shunt :: [Token] -> [Token] -> [Token]
shunt ops [] = ops
shunt ops (x:xs)
  | x == "("                    = shunt (x:ops) xs
  | x == ")"                    = reverse (takeWhile (/="(") ops) ++ shunt (drop 1 (dropWhile (/="(") ops)) xs
  | isOperator (head x) == True = if null ops || (head ops) == "(" || precedence x > precedence (head ops) 
                                  then shunt (x:ops) xs
                                  else (reverse (takeUntilSmallerPrec x ops)) ++ (shunt (x:(drop (length (takeUntilSmallerPrec x ops)) ops)) xs)
  | otherwise                   = x : (shunt ops xs)
