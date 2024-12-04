import Data.List (sort)

processLists :: [(Int, Int)] -> [Int]
processLists pairs = 
    let (listA, listB) = unzip pairs
    in map (\x -> x * (length $ filter (==x) listB)) listA

parseLine :: String -> (Int, Int)
parseLine line = 
    let [a, b] = map read (words line)
    in (a, b)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let pairs = map parseLine (lines contents)
    
    let differences = sum $ processLists pairs

    print differences

