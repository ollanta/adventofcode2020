import Text.Parsec
import qualified Data.HashMap.Strict as M


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


data Rule = Rule String [(Integer, Integer)]
  deriving (Eq, Show)


readD :: String -> ([Rule], [Integer], [[Integer]])
readD s = inp
  where
    Right inp = parse readInput "" s

    readInput = do
      rules <- readRule `endBy` newline
      newline
      string "your ticket:" >> newline
      yours <- readTicket <* newline
      newline
      string "nearby tickets:" >> newline
      nearby <- readTicket `endBy` newline
      return (rules, yours, nearby)

    readTicket = map read <$> many1 digit `sepBy`  string ","

    readRule = do
      name <- many1 (letter <|> char ' ')
      string ": "
      ranges <- readRange `sepBy` string " or "
      return $ Rule name ranges

    readRange = do
      lower <- many1 digit
      string "-"
      higher <- many1 digit
      return (read lower, read higher)
      

solve (rules, _, nearby) = sum . filter invalid . concat $ nearby
  where
    invalid v = not . any (inRuleRange v) $ rules

    inRuleRange v (Rule _ ranges) = any (inRange v) $ ranges

    inRange v (l,h)
      | l <= v && v <= h = True
      | otherwise        = False
