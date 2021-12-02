problem number = do
    fileContent <- readFile "input"
    print $ number (map format (lines fileContent))

firstProblem formattedCoordinates = takeRoute formattedCoordinates 0 0
  
secondProblem formattedCoordinates = takeRoute2 formattedCoordinates 0 0 0

format :: String -> (String, Int)
format string = (w!!0, read $ (w!!1))
  where w = words string

type Depth = Int
type Forward = Int
takeRoute :: [(String, Int)] -> Forward -> Depth -> Int
takeRoute [] fwd dpt = fwd * dpt
takeRoute (x:xs) fwd dpt 
  | fst x == "down" = takeRoute xs fwd (dpt + snd x)
  | fst x == "up" = takeRoute xs fwd (dpt - snd x)
  | otherwise = takeRoute xs (fwd + snd x) dpt

type Aim = Int
takeRoute2 :: [(String, Int)] -> Forward -> Depth -> Aim -> Int
takeRoute2 [] fwd dpt _ = fwd * dpt
takeRoute2 (x:xs) fwd dpt aim
  | fst x == "down" = takeRoute2 xs fwd dpt (aim + snd x)
  | fst x == "up" = takeRoute2 xs fwd dpt (aim - snd x)
  | otherwise = takeRoute2 xs (fwd + snd x) (dpt + aim*(snd x)) aim 