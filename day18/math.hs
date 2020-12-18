import Text.Parsec
import qualified Data.HashMap.Strict as M


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


data Term = ITerm Integer | BTerm [Term] | OpTerm Char
  deriving (Eq, Show)


readD :: String -> [[Term]]
readD s = eqs
  where
    Right eqs = parse (readEq `endBy` newline) "" s

    readEq = readTerm `sepBy` (string " ")

    readTerm = choice [readOpTerm, readBTerm, readITerm]

    readOpTerm = OpTerm <$> oneOf "+*"

    readBTerm = BTerm <$> between (string "(") (string ")") readEq

    readITerm = ITerm . read <$> many1 digit



solve eqs = sum . map (eval 0 '+') $ eqs
  where
    eval acc op (ITerm x:es) = eval (evalop acc op x) op es
    eval acc op (BTerm bes:es) = eval (evalop acc op bres) op es
      where
        bres = eval 0 '+' bes
    eval acc op (OpTerm c:es) = eval acc c es
    eval acc op [] = acc

    evalop x '+' y = x+y
    evalop x '*' y = x*y
