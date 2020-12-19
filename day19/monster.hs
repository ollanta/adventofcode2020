import Text.Parsec
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

    readRChar = do
      string "\""
      rchar <- letter
      string "\""
      return $ RChar rchar

    readRSeq = RSeq . map (map read) <$> (many1 digit `sepEndBy` string " ") `sepBy` string "| "


solve (rules, messages) = length . filter ((elem "") . matches rule0) $ messages
  where
    ruleM = M.fromList rules
    rule0 = ruleM M.! 0

    matches :: Rule -> String -> [String]
    matches _ [] = []
    matches (RChar c) (m:ms)
      | c /= m    = []
      | otherwise = [ms]
    matches (RSeq rnss) ms = concatMap matchesOption rnss
      where
        matchesOption rns = matchesSeq (map (ruleM M.!) rns) ms

    matchesSeq (r:rs) ms = concatMap (matchesSeq rs) mss
      where
        mss = matches r ms
    matchesSeq [] ms = [ms]
