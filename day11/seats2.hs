import Data.List
import qualified Data.HashMap.Strict as M

main :: IO ()
main = do
  interact (show . solve . lines)
  putStrLn ""


solve :: [String] -> Int
solve seats = M.size . M.filter (=='#') $ fixm
  where
    minit = M.fromList [((y,x), c) |
                        (y, row) <- zip [0..] seats,
                        (x, c) <- zip [0..] row]

    fms :: [M.HashMap (Integer, Integer) Char]
    fms = iterate update minit

    fixm = fst . head . dropWhile (uncurry (/=)) $ zip fms (tail fms)

    update sm = M.mapWithKey updateS sm
      where
        updateS c '#'
          | occNeighbours sm c >= 5 = 'L'
        updateS c 'L'
          | occNeighbours sm c == 0 = '#'
        updateS c v = v

    occNeighbours sm (y,x) = length . filter (=='#') $ neighbourSeats
      where
        neighbourSeats  = map findFirst dirs
        dirs = [(dy, dx) |
                 dy <- [-1..1],
                 dx <- [-1..1],
                 (dy, dx) /= (0, 0)]
        findFirst (dy,dx) = head . dropWhile (=='.') $ seatsInDir
          where
            seatsInDir = map (\c' -> M.lookupDefault 'L' c' sm) [(y+n*dy, x+n*dx) | n <- [1..]]

