problem number = do
    fileContent <- readFile "input"
    print $ number $ map(\x -> read x :: Int) $ lines fileContent

firstProblem = countIncrease

secondProblem = countIncrease . createSumOfTriplets 

countIncrease :: [Int] -> Int
countIncrease (x:y:xs)
  | y > x = 1 + countIncrease(y:xs)
  | otherwise = countIncrease(y:xs)
countIncrease _ = 0

createSumOfTriplets :: [Int] -> [Int]
createSumOfTriplets (x:y:z:xs) = (x+y+z) : createSumOfTriplets (y:z:xs)
createSumOfTriplets _ = [] 
