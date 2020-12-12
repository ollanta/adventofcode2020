import Text.Parsec
import Linear.Matrix
import Linear.V2
import Linear.Vector

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


solve instrs = fst $ foldl execI (V2 0 0, V2 10 1) instrs
  where
    rotM = V2 (V2 0 1) (V2 (-1) 0)
    getDir :: Char -> V2 Int
    getDir c = snd . head . filter ((==c) . fst) $ zip "NESW" dirs
      where
        dirs = iterate (rotM !*) (V2 0 1)

    execI :: (V2 Int, V2 Int) -> (Char, Int) -> (V2 Int, V2 Int)
    execI (v, vw) (c, n)
      | c `elem` "NSEW" = (v, vw + dir ^* n)
      | c == 'R'        = (v, rotate rotM)
      | c == 'L'        = (v, rotate (-rotM))
      | c == 'F'        = (v + (vw ^* n), vw)
      where
        dir = getDir c
        rotate rM = iterate (rM !*) vw !! div n 90

showS (V2 x y) = show (abs x + abs y)
