import Text.Parsec
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


type Tile = [String]


readD :: String -> [(Integer, Tile)]
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


solve tiles = countNessies prunedTile
  where
    tileMap = M.fromList tiles
    
    edgeMap = M.fromListWith (++) [(edge, [name]) |
                                   (name, tile) <- tiles,
                                   edge <- genEdges tile]

    uniqueEdges = M.map head . M.filter ((==1) . length) $ edgeMap

    uniqueByTile = M.fromListWith (+) $ zip (M.elems uniqueEdges) (repeat 1)

    cornerTiles = M.keys . M.filter (==4) $ uniqueByTile

    neighbourPairs = [(name, neighbour) |
                       nList <- M.elems edgeMap,
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
          | x == 0    = True
          | otherwise = map last leftTile == map head tileV
          where
            leftTile = m M.! (x-1,y)
              
        matchesUp tileV
          | y == 0    = True
          | otherwise = last upperTile == head tileV
          where
            upperTile = m M.! (x,y-1)

    prunedSMap = M.map (middle . map middle) smap

    middle (c:cs) = dropEnd cs
      where
        dropEnd [c] = []
        dropEnd (c:cs) = c:dropEnd cs

    prunedTile = showM prunedSMap


countNessies tile = (maxfound, dirt)
  where
    dirt = countHash tile - maxfound * countHash nessie
    countHash tile = length . concatMap (filter (=='#')) $ tile

    nessie = ["                  # ",
              "#    ##    ##    ###",
              " #  #  #  #  #  #   "]

    tsize = length tile
    nwidth = head . map length $ nessie
    nheight = length nessie

    maxfound = maximum . map findIn $ transformations tile
    findIn tile = length [(x,y) |
                           x <- [0..tsize-nwidth-1],
                           y <- [0..tsize-nheight-1],
                           isNessie (x,y) tile]

    isNessie (x,y) tile = all (=='#') [(tile !! (y+ny)) !! (x+nx) |
                                       (ny, nrow) <- zip [0..] nessie,
                                       (nx, c) <- zip [0..] nrow,
                                       c == '#']


showM :: M.HashMap (Integer,Integer) [String] -> [String]
showM map = [concat [map M.! (mx,my) !! row | mx <- [0..sizeM]] |
             my <- [0..sizeM],
             row <- [0..sizeI-1]]
  where
    sizeM = fst . maximum . M.keys $ map
    sizeI = length . head . M.elems $ map


genEdges tile = edges ++ map reverse edges
  where
    edges = [head tile, last tile, map head tile, map last tile]


transformations tile = [tile''' |
                        tile' <- [tile, reverse tile],
                        tile'' <- [tile', map reverse tile'],
                        tile''' <- take 4 . iterate rotate $ tile'']


rotate tile = [[row !! ix | row <- tile] |
               ix <- [size-1,size-2..0]]
  where
    size = length tile
