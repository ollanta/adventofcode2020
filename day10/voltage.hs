import Data.List

main :: IO ()
main = do
  interact (show . solve . map read . lines)
  putStrLn ""


solve :: [Integer] -> (Integer, Integer, Integer)
solve volts = (c1, c3, c1*c3)
  where
    svolts = sort (0 : 3 + maximum volts : volts)
    diffs = zipWith (-) (tail svolts) svolts

    c1 = toInteger $ count 1 diffs
    c3 = toInteger $ count 3 diffs

    count n ls = length . filter (==n) $ ls
