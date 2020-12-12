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


solve instrs = foldl execI (0,0,(10,1)) instrs
  where
    execI (x,y,(wx,wy)) ('N', n) = (x,y,(wx,wy+n))
    execI (x,y,(wx,wy)) ('S', n) = (x,y,(wx,wy-n))
    execI (x,y,(wx,wy)) ('E', n) = (x,y,(wx+n,wy))
    execI (x,y,(wx,wy)) ('W', n) = (x,y,(wx-n,wy))
    execI (x,y,w) ('R', n) = (x,y,turn w n)
    execI (x,y,w) ('L', n) = (x,y,turn w (-n))
    execI (x,y,(wx,wy)) ('F', n) = (x+wx*n,y+wy*n,(wx,wy))

    turn :: (Int, Int) -> Int -> (Int, Int)
    turn c 0 = c
    turn (x,y) n
      | n > 0 = turn (y,-x) (n-90)
      | n < 0 = turn (-y,x) (n+90)


showS (a,b,c) = show $ abs a + abs b
