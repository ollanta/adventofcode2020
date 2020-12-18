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



solve eqs = sum . map adveval $ eqs
  where
    adveval eq = result
      where
        (ITerm result:[]) = evalMult . evalPlus $ eq
        
        evalPlus (t1:OpTerm '+':t2:ts) = evalPlus (ITerm (evalTerm t1 + evalTerm t2):ts)
        evalPlus (t:ts) = t:evalPlus ts
        evalPlus [] = []

        evalMult (t1:OpTerm '*':t2:ts) = evalMult (ITerm (evalTerm t1 * evalTerm t2):ts)
        evalMult (t:ts) = t:evalMult ts
        evalMult [] = []
          
    evalTerm (ITerm x) = x
    evalTerm (BTerm bes) = adveval bes
