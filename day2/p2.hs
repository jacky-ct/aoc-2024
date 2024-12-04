checkPredicateWithRemovalNested :: ([Int] -> Bool) -> [[Int]] -> Bool
checkPredicateWithRemovalNested pred lists = 
    or (map (checkPredicateWithRemoval pred) lists)
    where
        checkPredicateWithRemoval pred list = 
            any pred (map (\x -> take x list ++ drop (x+1) list) [0..(length list - 1)]) 

testValidity :: [Int] -> Bool
testValidity series = maxDiffThree && strictlyIncreasesOrDecreases
    where
        differences = zipWith (-) series (tail series)
        maxDiffThree = all ((<=3) . abs) differences
        strictlyIncreasesOrDecreases = (all (\x -> x > 0) differences) || (all (\x -> x < 0) differences)

getListWithRemoval :: [Int] -> [[Int]]
getListWithRemoval list = map (\x -> take x list ++ drop (x+1) list) [0..(length list - 1)]

parseLine :: String -> [Int]
parseLine line = map read (words line)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    
    let reports = map parseLine (lines contents)
    
    let allPermutations = map getListWithRemoval reports

    let validities = length (filter id ())
    
    print validities
