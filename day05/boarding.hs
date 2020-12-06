import Data.List


main :: IO ()
main = do
  interact (show . solve . lines)
  putStrLn ""


solve = maximum . map (getId 0)
  where
    getId :: Int -> String -> Int
    getId acc [] = acc
    getId acc (s:ls)
      | s == 'R' || s == 'B' = getId (1+acc*2) ls
      | otherwise            = getId (acc*2) ls
