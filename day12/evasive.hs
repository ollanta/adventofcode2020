import Text.Parsec


main :: IO ()
main = do
  interact (showS . solve . readD)
  putStrLn ""


readD :: String -> [(Char, Int)]
readD s = instrs
  where
    Right instrs = parse (readInstr `endBy` newline) "" s

    readInstr = do
      c <- oneOf "NSEWRLF"
      n <- many1 digit
      return (c, read n)


solve instrs = foldl execI (0,0,'E') instrs
  where
    execI (x,y,d) ('N', n) = (x,y+n,d)
    execI (x,y,d) ('S', n) = (x,y-n,d)
    execI (x,y,d) ('E', n) = (x+n,y,d)
    execI (x,y,d) ('W', n) = (x-n,y,d)
    execI (x,y,d) ('R', n) = (x,y,turn d n)
    execI (x,y,d) ('L', n) = (x,y,turn d (-n))
    execI (x,y,d) ('F', n) = execI (x,y,d) (d, n)

    turn :: Char -> Int -> Char
    turn d n = dirs !! div (abs n) 90
      where
        order = if n > 0 then "NESW" else "NWSE"
        dirs = dropWhile (/=d) . cycle $ order


showS (a,b,c) = show $ abs a + abs b
