import Text.Parsec
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S


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


solve tiles = result
  where
    tileMap = M.fromList tiles
    
    edgeIds = [(name, edgeId) | (name, tile) <- tiles, edgeId <- genEdgeIds tile]
    edgeIdMap = foldl (\m (n, eid) -> M.insertWith (++) eid [n] m) M.empty edgeIds

    uniqueEdges = M.map head . M.filter ((==1) . length) $ edgeIdMap
    uniqueByTile = foldl (\m (en, n) -> M.insertWith (+) n 1 m) M.empty (M.toList uniqueEdges)

    cornerTiles = M.keys . M.filter (==4) $ uniqueByTile

    neighbourPairs = [(name, neighbour) |
                       nList <- M.elems edgeIdMap,
                       length nList /= 1,
                       name <- nList,
                       neighbour <- nList,
                       name /= neighbour]
    neighbourMap = foldl (\m (name, neigh) -> M.insertWith S.union name (S.singleton neigh) m) M.empty neighbourPairs

    size = toInteger . head . filter (\k -> k*k==length tiles) $ [2..]
    initCorner = head cornerTiles
    initM = (M.insert (0,0) initCorner M.empty)
    eligibleMaps = genMap (1,0) (S.singleton initCorner) initM
    genMap :: (Integer,Integer) -> S.HashSet Integer -> M.HashMap (Integer,Integer) Integer -> [M.HashMap (Integer,Integer) Integer]
    genMap c used m
      | done       = [m]
      | otherwise  = [m' |
                      t  <- S.toList (fits c),
                      not (S.member t used),
                      m' <- genMap (nextC c) (S.insert t used) (M.insert c t m)]
      where
        done = (toInteger $ M.size m) == size*size

        nextC (x,y)
          | x < size-1 = (x+1, y)
          | otherwise  = (0, y+1)

        fits c = foldr1 (S.intersection) . map (neighbourMap M.!) $ inPlace
          where
            inPlace = map (m M.!) . filter (`M.member` m) $ adjacent c

        adjacent (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

    emap = head eligibleMaps
    coords = [(x,y) | y <- [0..size-1], x <- [0..size-1]]
    --smap :: [M.HashMap (Integer,Integer) [String]]
    smap = head $ genSMap coords M.empty

    genSMap :: [(Integer,Integer)] -> M.HashMap (Integer,Integer) [String] -> [M.HashMap (Integer,Integer) [String]]
    genSMap [] m = [m]
    genSMap (c:cs) m = [m' |
                        tile' <- tilings,
                        m' <- genSMap cs (M.insert c tile' m)]
      where
        tileName = emap M.! c
        tile = tileMap M.! tileName
        tileVariants = transformations tile

        tilings = filter matchesLeft . filter matchesUp $ tileVariants

        (x,y) = c
        matchesLeft tileV
          | x == 0    = isUnique . leftEdge $ tileV
          | otherwise = rightEdge leftTile == leftEdge tileV
          where
            leftTile = m M.! (x-1,y)
            rightEdge = map last
            leftEdge = map head
              
        matchesUp tileV
          | y == 0    = isUnique . topEdge $ tileV
          | otherwise = bottomEdge upperTile == topEdge tileV
          where
            upperTile = m M.! (x,y-1)
            topEdge = head
            bottomEdge = last

        isUnique edge = M.member (toEId edge) uniqueEdges

    prunedSMap = M.map (middle . map middle) smap

    middle (c:cs) = dropEnd cs
      where
        dropEnd [c] = []
        dropEnd (c:cs) = c:dropEnd cs

    prunedTile = showM prunedSMap

    nessie = ["                  # ",
              "#    ##    ##    ###",
              " #  #  #  #  #  #   "]
    countNessies tile = (length found, dirt)
      where
        dirt = countHash tile - (length found) * countHash nessie

        tsize = length tile
        nwidth = head . map length $ nessie
        nheight = length nessie

        found = [(x,y) |
                  x <- [0..tsize-nwidth-1],
                  y <- [0..tsize-nheight-1],
                  isNessie (x,y)]

        isNessie (x,y) = and [(tile !! (y+ny)) !! (x+nx) == '#' |
                              (ny, nrow) <- zip [0..] nessie,
                              (nx, c) <- zip [0..] nrow,
                              c == '#']

        countHash tile = length . concatMap (filter (=='#')) $ tile
                          
    result = maximum $ map countNessies (transformations prunedTile)



showM :: M.HashMap (Integer,Integer) [String] -> [String]
showM map = [concat [map M.! (mx,my) !! row | mx <- [0..sizeM]] |
             my <- [0..sizeM],
             row <- [0..sizeI-1]]
  where
    sizeM = fst . maximum . M.keys $ map
    sizeI = length . head . M.elems $ map


genEdgeIds :: [String] -> [Integer]
genEdgeIds tile = map toEId edges ++ map (toEId . reverse) edges
  where
    edges = [head tile, last tile, map head tile, map last tile]
    

toEId string = sum $ zipWith (\c p -> toBit c * 2^p) string [0..]
  where
    toBit '.' = 0
    toBit '#' = 1


transformations tile = [tile''' |
                        tile' <- [tile, reverse tile],
                        tile'' <- [tile', map reverse tile'],
                        tile''' <- take 4 . iterate rotate $ tile'']

rotate tile = [[row !! ix | row <- tile] |
               ix <- [size-1,size-2..0]]
  where
    size = length tile
