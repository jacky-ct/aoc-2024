import Data.List (sort)

-- Pure function to process the lists
processLists :: [(Int, Int)] -> [Int]
processLists pairs = 
    let (listA, listB) = unzip pairs
        sortedA = sort listA
        sortedB = sort listB
    in zipWith (\x y -> abs (x - y)) sortedA sortedB

-- IO function to read and process files
main :: IO ()
main = do
    -- Read the file
    contents <- readFile "input.txt"
    
    -- Parse the input
    let pairs = map parseLine (lines contents)
    
    -- Apply pure processing function
    let differences = sum $ processLists pairs
    
    -- Print results
    print differences

-- Pure parsing function
parseLine :: String -> (Int, Int)
parseLine line = 
    let [a, b] = map read (words line)
    in (a, b)
