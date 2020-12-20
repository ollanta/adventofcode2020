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
    edgeNs = [(name, edge) | (name, tile) <- tiles, edge <- genEdgeN tile]

    edgeNMap = foldl (\m (n, e) -> M.insertWith (++) e [n] m) M.empty edgeNs

    uniqueEdges = M.map head . M.filter ((==1) . length) $ edgeNMap

    uniqueByTile = foldl (\m (en, n) -> M.insertWith (+) n 1 m) M.empty (M.toList uniqueEdges)


genEdgeN :: [String] -> [Integer]
genEdgeN tile = map toNumber edges ++ map (toNumber . reverse) edges
  where
    edges = [head tile, last tile, map head tile, map last tile]
    


toNumber string = sum $ zipWith (\c p -> toBit c * 2^p) string [0..]
  where
    toBit '.' = 0
    toBit '#' = 1
