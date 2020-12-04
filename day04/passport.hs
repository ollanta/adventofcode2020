import Text.Parsec
import qualified Data.HashMap.Strict as M


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""

data Field = Field String String
  deriving (Show, Eq)

readD :: String -> [[Field]]
readD s = fields
  where
    Right fields = parse (readP `sepBy` newline) "" s

    readP = readField `endBy` space

    readField = do
      fieldname <- many1 letter
      char ':'
      fielddata <- many1 (choice [alphaNum, oneOf "&#"])
      return $ Field fieldname fielddata


validPassport :: [Field] -> Bool
validPassport pp = all (hasField pp) requiredFields
  where
    hasField passport rfn = any (\(Field fn _) -> fn == rfn) passport

    requiredFields = ["ecl", "eyr", "hcl", "byr", "iyr", "pid", "hgt"]

solve passports = length . filter validPassport $ passports
