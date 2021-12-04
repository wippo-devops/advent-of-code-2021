import Data.Char (digitToInt)
import Data.List (foldl')


problem number = do
    fileContent <- readFile "input"
    print $ number (lines fileContent) 

firstProblem :: [String] -> Int
firstProblem arr = b*c
    where a = getAllCounts arr ""
          b = toDec a
          c = toDec (negateBinary a)
  
secondProblem :: [String] -> Int
secondProblem arr = (toDec a) * (toDec b)
    where a = getRating arr '>' 0 --o2
          b = getRating arr 'o' 0 --co2

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

negateBinary :: String -> String
negateBinary [] = []
negateBinary (x:xs)
  | x == '0' = "1" ++ negateBinary xs
  | otherwise= "0" ++ negateBinary xs

cmp :: Char -> Int -> Int -> Bool
cmp '>' a b = a > b
cmp _ a b = a <= b

getRating :: [String] -> Char -> Int -> String
getRating [] _ _ = ""
getRating arr comparator idx 
  | length arr == 1 = head arr 
  | cmp comparator zeroes ones = getRating (filterByCharOnIndex arr '0' idx) comparator (idx + 1)
  | otherwise = getRating (filterByCharOnIndex arr '1' idx) comparator (idx + 1)
  where firstElems = map (\n -> [n !! idx]) arr
        zeroes = countZeros firstElems
        ones = countOnes firstElems

filterByCharOnIndex :: [String] -> Char -> Int -> [String]
filterByCharOnIndex arr char idx = filter (\n -> n !! idx == char) arr

getAllCounts :: [String] -> String -> String
getAllCounts [] s = s
getAllCounts arr s 
  | (length $ head arr) == 0 = s
  | countZeros firstElems >= (countOnes firstElems) = getAllCounts newArr (s ++ "0")
  | otherwise = getAllCounts newArr (s ++ "1")
  where newArr = map (drop 1) arr
        firstElems = map (take 1) arr

count :: [String] -> String -> Int
count array s = length $ filter (==s) array

countZeros :: [String] -> Int
countZeros arr = count arr "0"

countOnes :: [String] -> Int
countOnes arr = count arr "1"