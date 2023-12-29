import Text.Parsec
import qualified Data.HashMap.Strict as M
import qualified Data.HashMap.Lazy as L
import Data.Either
import qualified Text.ParserCombinators.ReadP as P

main :: IO ()
main = do
  --interact (unlines . map show . solve . readD)
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


{-
-- This is based on Parsec which doesn't implement a fully backtracking choice operator
-- so this will fail for more complicated rules (like part 2)

solve (rules, messages) = length . filter matches0 $ messages
  where
    ruleM = M.fromList [(name, toParsec rule) | (name, rule) <- rules]

    toParsec :: Rule -> Parsec String () String
    toParsec (RChar c) = string (c:[])
    toParsec (RSeq options) = choice [try $ foldr1 (<>) rules |
                                      option <- options,
                                      let rules = map (ruleM M.!) option]

    matches0 message = isRight $ parse (ruleM M.! 0 <* eof) "" message


-}
--

solve (rules, messages) = length . filter matches0 $ messages
  where
    ruleM = L.fromList [(name, toParserC rule) | (name, rule) <- rules]

    toParserC :: Rule -> P.ReadP Char
    toParserC (RChar c)      = P.char c
    toParserC (RSeq options) = P.choice [foldr1 (<*) rules |
                                         option <- options,
                                         let rules = map (ruleM L.!) option]

    matches0 message = not . null . P.readP_to_S (ruleM L.! 0 <* P.eof) $ message
