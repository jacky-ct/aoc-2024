import Data.Char (isDigit)
import Data.List
import Text.Read (readMaybe)

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

  let intPairs = map getInts ((map (\x -> removePrefix "mul(" x) (allStringsWithPrefix "mul(" contents)))
  let intProducts = [x * y | Just (x, y) <- intPairs]
  print $ sum intProducts
