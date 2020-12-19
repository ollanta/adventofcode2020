import Text.Parsec
import Text.Parsec.Token
import qualified Data.HashMap.Strict as M


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


data Rule = RChar Char | RSeq [[Integer]]
  deriving (Eq, Show)


readD :: String -> ([(Integer, Rule)], [String])
readD s = rules
  where
    Right rules = parse readInput "" s

    readInput = do
      rules <- readRule `endBy` newline
      newline
      messages <- many1 letter `endBy` newline
      return $ (rules, messages)

    readRule = do
      name <- many1 digit
      string ": "
      rule <- readRChar <|> readRSeq
      return (read name, rule)

    readRChar = RChar <$> between (char '"') (char '"') letter

    readRSeq = RSeq . map (map read) <$> (many1 digit `sepEndBy` string " ") `sepBy` string "| "


solve (rules, messages) = length . filter (matches (ruleM M.! 0)) $ messages
  where
    ruleM = M.fromList rules
    rule0 = ruleM M.! 0

    matches :: Rule -> String -> Bool
    matches rule message = any (=="") unmatchedSuffixes
      where
        unmatchedSuffixes = matchRule rule message

    matchRule _ [] = []
    matchRule (RChar c) (m:ms)
      | c /= m    = []
      | otherwise = [ms]
    matchRule (RSeq options) ms = [suffix |
                                   ruleIxs <- options,
                                   suffix <- matchRuleSeq (map (ruleM M.!) ruleIxs) ms]

    matchRuleSeq (r:rs) ms = [suffix' |
                              suffix <- matchRule r ms,
                              suffix' <- matchRuleSeq rs suffix]
    matchRuleSeq [] ms = [ms]
