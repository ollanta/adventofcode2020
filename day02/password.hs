import Text.Parsec

main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""

readD :: String -> [(Integer, Integer, Char, String)]
readD s = parsed
  where
    Right parsed = parse (read' `endBy` newline) "" s
    read' = do
      lb <- many digit
      char '-'
      ub <- many digit
      space
      c <- letter
      char ':'
      space
      pass <- many letter
      return (read lb, read ub, c, pass)

solve ps = length . filter valid $ ps

valid (lb, ub, c, pass) = lb <= nc && nc <= ub
  where
    nc = toInteger . length . filter (==c) $ pass
