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


solve rules = sizeOf "shiny gold" - 1
  where
    rulemap = M.fromList rules

    sizeOf bagcolor
      | null newbags = 1
      | otherwise = 1 + (sum . map (\(n, bc) -> n * sizeOf bc) $ newbags)
      where
        newbags = rulemap M.! bagcolor
