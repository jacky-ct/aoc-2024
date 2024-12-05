import Data.Char (isDigit)
import Data.List
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

filterDont :: String -> String
filterDont str = case hasDont str of
  False -> str
  True -> filterDont (removeDont str)
  where
    hasDont str = isInfixOf "don't()" str
    hasDo str = isInfixOf "do()" str
    removeDont str = case hasDo str of
      -- Pretty sure it's this line that's not quite working idk what's up
      True -> head (splitOn "don't()" str) ++ head (filter (\x -> isPrefixOf "do()" x) (tails str))
      False -> head (splitOn "don't()" str)

allStringsWithPrefix :: String -> String -> [String]
allStringsWithPrefix prefix string = filter (\x -> isPrefixOf prefix x) (tails string)

removePrefix :: String -> String -> String
removePrefix prefix string = drop (length prefix) string

getInts :: String -> Maybe (Int, Int)
getInts str = do
  (first, rest1) <- takeInt str
  rest2 <- checkComma rest1
  (second, rest3) <- takeInt rest2
  _ <- checkBracket rest3
  return (first, second)
  where
    takeInt :: String -> Maybe (Int, String)
    takeInt str =
      let digits = takeWhile isDigit str
          rest = dropWhile isDigit str
       in if length digits < 4 && length digits > 0
            then case readMaybe digits of
              Just n -> Just (n, rest)
              Nothing -> Nothing
            else Nothing

    checkComma :: String -> Maybe (String)
    checkComma (',' : rest) = Just rest
    checkComma _ = Nothing

    checkBracket :: String -> Maybe (String)
    checkBracket (')' : rest) = Just rest
    checkBracket _ = Nothing

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ head (filter (\x -> isPrefixOf "do()" x) (tails test))

  let filteredString = filterDont contents
  let intPairs = map getInts ((map (\x -> removePrefix "mul(" x) (allStringsWithPrefix "mul(" filteredString)))
  let intProducts = [x * y | Just (x, y) <- intPairs]

  print $ sum intProducts
