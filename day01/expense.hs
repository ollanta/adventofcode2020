
main :: IO ()
main = interact run
  where run = show . solve . map read . lines

solve :: [Integer] -> Integer
solve (i:is) = case solve' is of
  Just ans -> ans
  Nothing -> solve is
  where
    solve' (j:js)
      | j + i == 2020 = Just (j * i)
      | otherwise     = solve' js
    solve' _ = Nothing

