import Text.Parsec
import qualified Data.HashMap.Strict as M

main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


readD :: String -> [[String]]
readD s = decks
  where
    Right decks = parse (readDir `sepEndBy` newline) "" s

    readDir = many1 . choice . map (try . string) $ ["e", "se", "sw", "w", "nw", "ne"]


solve dirs = M.size . M.filter odd $ tileMap
  where
    tiles = map (toCoord (0,0)) dirs

    tileMap :: M.HashMap (Int, Int) Int
    tileMap = M.fromListWith (+) $ zip tiles (repeat 1)
    
    toCoord c [] = c
    toCoord c (d:ds) = toCoord (move c d) ds

    move (x,y) d
      | d == "e" = (x+1,y)
      | d == "w" = (x-1,y)
      | d == "se" = (x, y-1)
      | d == "sw" = (x-1, y-1)
      | d == "nw" = (x, y+1)
      | d == "ne" = (x+1, y+1)
