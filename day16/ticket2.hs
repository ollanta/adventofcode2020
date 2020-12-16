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
      string "your ticket:"
      newline
      yours <- readTicket
      newline
      newline
      string "nearby tickets:"
      newline
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
      

solve (rules, yours, nearby) = product . map snd . filter departure $ yoursOrdered
  where
    yoursOrdered = zip ruleOrder yours
    departure ((Rule name _), _)
      | take 9 name == "departure" = True
      | otherwise = False
    
    ruleOrder = findRuleOrder rules validTickets
    
    validTickets = filter (not . any invalid) nearby
    
    anyInvalid ticket = any invalid ticket

    invalid v = not . any (inRuleRange v) $ rules

inRuleRange v (Rule _ ranges) = any (inRange v) $ ranges
  where
    inRange v (l,h)
      | l <= v && v <= h = True
      | otherwise        = False


findRuleOrder rules tickets = singular possible
  where
    singular ordering
      | length decided == length ordering = decided
      | otherwise = singular ordering'
      where
        decided = map head . filter ((1==) . length) $ ordering
        ordering' = map reduce ordering

        reduce [r] = [r]
        reduce rs  = filter (not . (`elem` decided)) rs
    
    possible = foldl rinse (repeat rules) tickets
    
    rinse _ [] = []
    rinse (rs:rss) (f:fs) = rs' : rinse rss fs
      where
        rs' = filter (inRuleRange f) rs
