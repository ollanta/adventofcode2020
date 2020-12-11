import Data.List
import qualified Data.HashMap as M

main :: IO ()
main = do
  interact (show . s' . solve . lines)
  putStrLn ""

s' = M.size . M.filter (=='#') . snd

solve :: [String] -> (Integer, M.Map (Integer,Integer) Char)
solve seats = head . dropWhile ((>0) . fst) $ ms
  where
    m = M.fromList [((y,x), c) | (y, row) <- zip [0..] seats, (x, c) <- zip [0..] row]
    rows = length seats
    cols = length (head seats)

    ms = iterate (\(_, m) -> update m) (1, m)

    update sm = M.mapAccumWithKey (updateS sm) 0 sm

    updateS sm a c '.' = (a, '.')
    updateS sm a c '#'
      | neighbours sm c '#' >= 5 = (a+1, 'L')
      | otherwise = (a, '#')
    updateS sm a c 'L'
      | neighbours sm c '#' == 0 = (a+1, '#')
      | otherwise = (a, 'L')

    neighbours sm (y,x) t = length . filter (==t) . map (\dir -> findFirst dir) $ dirs
      where
        dirs = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]
        findFirst (dy,dx) = head . dropWhile (=='.') $ inDir
          where
            inDir = map (\c' -> M.findWithDefault 'L' c' sm) [(y+n*dy, x+n*dx) | n <- [1..]]

