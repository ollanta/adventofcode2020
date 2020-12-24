import Text.Parsec
import qualified Data.HashMap.Strict as M

main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


directions = ["e", "se", "sw", "w", "nw", "ne"]


readD :: String -> [[String]]
readD s = decks
  where
    Right decks = parse (readDir `sepEndBy` newline) "" s

    readDir = many1 . choice . map (try . string) $ directions


solve dirs = M.size . head . drop 100 . iterate evolve $ initBlacks
  where
    tiles = map (toCoord (0,0)) dirs

    tileMap :: M.HashMap (Int, Int) Int
    tileMap = M.fromListWith (+) $ zip tiles (repeat 1)
    initBlacks = M.filter odd $ tileMap
    
    toCoord c [] = c
    toCoord c (d:ds) = toCoord (move c d) ds

    move (x,y) d
      | d == "e" = (x+1,y)
      | d == "w" = (x-1,y)
      | d == "se" = (x, y-1)
      | d == "sw" = (x-1, y-1)
      | d == "nw" = (x, y+1)
      | d == "ne" = (x+1, y+1)

    evolve bmap = M.union (M.filter (\n -> n==1 || n==2) blackMap) (M.filter (==2) whiteMap)
      where
        neighbourMap = M.fromListWith (+) . (`zip` repeat 1) . concatMap neighbours . M.keys $ bmap
        neighbours c = map (move c) directions

        whiteMap = M.difference neighbourMap bmap
        blackMap = M.intersection neighbourMap bmap
