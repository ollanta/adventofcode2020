import Text.Parsec
import Data.List


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""

readD :: String -> [[String]]
readD s = fields
  where
    Right fields = parse (readP `sepBy` newline) "" s

    readP = readField `endBy` space

    readField = do
      chars <- many1 letter
      return $ chars


solve forms = sum . map countUnique $ forms
  where
    countUnique = length . nub . concat
