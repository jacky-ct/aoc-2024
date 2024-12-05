import Data.List (sort)

processLists :: [(Int, Int)] -> [Int]
processLists pairs =
  let (listA, listB) = unzip pairs
      sortedA = sort listA
      sortedB = sort listB
   in zipWith (\x y -> abs (x - y)) sortedA sortedB

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let pairs = map parseLine (lines contents)
  let differences = sum $ processLists pairs
  print differences

parseLine :: String -> (Int, Int)
parseLine line =
  let [a, b] = map read (words line)
   in (a, b)
