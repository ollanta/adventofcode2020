import Text.Parsec
import qualified Data.HashMap.Strict as M
import Data.List

main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""

readD :: String -> [(String, [(Int, String)])]
readD s = rules
  where
    Right rules = parse (readRule `endBy` newline) "" s

    word = many1 letter
    color = word <> count 1 space <> word

    readRule = do
      container <- color
      string " bags contain "
      containee <- choice [readC `sepBy1` string (", "), readN]
      string "."
      return $ (container, containee)

    readN = string "no other bags" *> pure []
    readC = do
      amount <- many1 digit
      space
      bagtype <- color
      string " bag"
      optional (string "s")
      return (read amount, bagtype)


solve rules = length . filter (canHoldGold . fst) $ rules
  where
    rulemap = M.fromList rules

    canHoldGold bagcolor
      | null newbags = False
      | any (=="shiny gold") newbagcolors = True
      | otherwise = any canHoldGold newbagcolors
      where
        newbags = rulemap M.! bagcolor
        newbagcolors = map snd newbags
