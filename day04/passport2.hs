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
validPassport pp = all validField pp && all (hasField pp) requiredFields
  where
    hasField passport rfn = any (\(Field fn _) -> fn == rfn) passport

    requiredFields = ["ecl", "eyr", "hcl", "byr", "iyr", "pid", "hgt"]


validField :: Field -> Bool
validField (Field "byr" s) = length s == 4 && byr >= 1920 && byr <= 2002
  where
    byr = read s
validField (Field "iyr" s) = length s == 4 && iyr >= 2010 && iyr <= 2020
  where
    iyr = read s
validField (Field "eyr" s) = length s == 4 && eyr >= 2020 && eyr <= 2030
  where
    eyr = read s
validField (Field "hgt" s) = case hgt of
  Left _ -> False
  Right hgt -> validHgt hgt
  where
    hgt = parse (hgtParse <* eof) "" s
    hgtParse = do
      height <- many1 digit
      measure <- choice [string "cm", string "in"]
      return (read height, measure)
    validHgt (h, "cm") = 150 <= h && h <= 193
    validHgt (h, "in") = 59 <= h && h <= 76
validField (Field "hcl" s) = case hcl of
  Left _ -> False
  Right _ -> True
  where
    hcl = parse (hclParse <* eof) "" s
    hclParse = do
      char '#'
      count 6 alphaNum
validField (Field "ecl" s) = any (==s) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validField (Field "pid" s) = case pid of
  Left _ -> False
  Right _ -> True
  where
    pid = parse (pidParse <* eof) "" s
    pidParse = do
      count 9 digit
validField _ = True


solve passports = length . filter validPassport $ passports
