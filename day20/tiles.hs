import Text.Parsec
import qualified Data.HashMap.Strict as M


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


readD :: String -> [(Integer, [String])]
readD s = tiles
  where
    Right tiles = parse (readTile `sepBy` newline) "" s

    readTile = do
      string "Tile "
      num <- many1 digit
      string ":"
      newline
      rows <- (many1 (oneOf ".#") `sepEndBy` newline)
      return $ (read num, rows)


solve tiles = product . M.keys . M.filter (==4) $ uniqueByTile
  where
    edgeMap = M.fromListWith (++) [(edge, [name]) |
                                   (name, tile) <- tiles,
                                   edge <- genEdges tile]

    uniqueEdges = M.map head . M.filter ((==1) . length) $ edgeMap

    uniqueByTile = M.fromListWith (+) $ zip (M.elems uniqueEdges) (repeat 1)


genEdges tile = edges ++ map reverse edges
  where
    edges = [head tile, last tile, map head tile, map last tile]
