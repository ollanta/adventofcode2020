import Data.List


main :: IO ()
main = do
  interact (show . solve . lines)
  putStrLn ""


solve seats = [minSeat..maxSeat] \\ idList
  where
    idList = map (getId 0) seats
    minSeat = minimum idList
    maxSeat = maximum idList
    
    getId :: Int -> String -> Int
    getId acc [] = acc
    getId acc (s:ls)
      | s == 'R' || s == 'B' = getId (1+acc*2) ls
      | otherwise            = getId (acc*2) ls
