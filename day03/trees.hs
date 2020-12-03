import Text.Parsec
import qualified Data.HashMap.Strict as M


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""

type Coord = (Int, Int)

data Point = Open | Tree
  deriving (Show, Eq)

readD :: String -> [[Point]]
readD s = points
  where
    Right points = parse ((`endBy` newline) $ many1 readP ) "" s
    readP = readC <$> oneOf ".#"

    readC '.' = Open
    readC '#' = Tree


toPmap :: [[Point]] -> M.HashMap Coord Point
toPmap allPs = M.fromList withCoords
  where
    withCoords = [((x,y), point) |
                  (y,row) <- zip [0..] allPs,
                  (x,point) <- zip [0..] row]


solve points = length . filter (==Tree) $ getSlope 3 points


getSlope dx points = [pmap M.! c | c <- zip xs ys]
  where
    pmap = toPmap points
    width = length . head $ points
    height = length points

    xs = map (`mod` width) $ [0, dx..]
    ys = [0..height-1]
