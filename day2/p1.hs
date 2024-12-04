testValidity :: [Int] -> Bool
testValidity series = maxDiffThree && strictlyIncreasesOrDecreases
    where
        differences = zipWith (-) series (tail series)
        maxDiffThree = all ((<=3) . abs) differences
        strictlyIncreasesOrDecreases = (all (\x -> x > 0) differences) || (all (\x -> x < 0) differences)

parseLine :: String -> [Int]
parseLine line = map read (words line)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    
    let reports = map parseLine (lines contents)
    
    let validities = length (filter id (map testValidity reports))
    
    print validities
