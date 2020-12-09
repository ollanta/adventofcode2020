

main :: IO ()
main = do
  interact (show . solve . map read . lines)
  putStrLn ""


solve cyph = checkValid (reverse . take n $ cyph) (drop n cyph)
  where
    n = 25
    checkValid ls (s:ss)
      | hasSum ls s = checkValid ls' ss
      | otherwise   = s
      where
        ls' = s : take (n-1) ls
    checkValid ls [] = -1

    hasSum ls s = s `elem` sums ls


sums [] = []
sums (l:ls) = map (l+) ls ++ sums ls
    
