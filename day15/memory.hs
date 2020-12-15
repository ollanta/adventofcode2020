import Text.Parsec
import qualified Data.HashMap.Strict as M


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


readD :: String -> [Int]
readD s = map read ops
  where
    Right ops = parse ((many1 digit) `sepBy` string ",") "" s
      

solve start = plays !! (2020 - 1)
  where
    plays = start ++ play minit (last start, length start)
    minit = M.fromList $ zip mstart [1..]
      where
        mstart = reverse . tail . reverse $ start

    play m (p, i)
      | not (M.member p m) = 0 : play m' (0, i+1)
      | otherwise          = p': play m' (p', i+1)
      where
        p' = i - m M.! p
        m' = M.insert p i m
    
