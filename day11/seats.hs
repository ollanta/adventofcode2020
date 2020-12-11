import Data.List
import qualified Data.HashMap.Strict as M

main :: IO ()
main = do
  interact (show . solve . lines)
  putStrLn ""


solve :: [String] -> Int
solve seats = M.size . M.filter (=='#') $ fixm
  where
    minit = M.fromList [((y,x), c) | (y, row) <- zip [0..] seats, (x, c) <- zip [0..] row]

    fms :: [M.HashMap (Integer, Integer) Char]
    fms = iterate update minit

    fixm = fst . head . dropWhile (uncurry (/=)) $ zip fms (tail fms)

    update sm = M.mapWithKey (updateS sm) sm

    updateS sm c '.' = '.'
    updateS sm c '#'
      | neighbours sm c '#' >= 4 = 'L'
      | otherwise = '#'
    updateS sm c 'L'
      | neighbours sm c '#' == 0 = '#'
      | otherwise = 'L'

    neighbours sm (y,x) t = length . filter (==t) $ neighbourSeats
      where
        neighbourSeats  = map (\nc -> M.lookupDefault '.' nc sm) $ neighbourCoords
        neighbourCoords = [(y+dy, x+dx) |
                           dy <- [-1..1],
                           dx <- [-1..1],
                           (dy, dx) /= (0, 0)]
