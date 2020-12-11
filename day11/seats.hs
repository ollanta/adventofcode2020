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
      | neighbours sm c '#' >= 4 = (a+1, 'L')
      | otherwise = (a, '#')
    updateS sm a c 'L'
      | neighbours sm c '#' == 0 = (a+1, '#')
      | otherwise = (a, 'L')

    neighbours sm (y,x) t = length . filter (==t) . map (\nc -> M.findWithDefault '.' nc sm) $ ncs
      where
        ncs = [(y-1,x-1), (y-1,x), (y-1,x+1), (y,x-1), (y,x+1), (y+1,x-1), (y+1,x), (y+1,x+1)]
