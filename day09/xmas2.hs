

main :: IO ()
main = do
  interact (show . solve . map read . lines)
  putStrLn ""


solve :: [Integer] -> Integer
solve cyph = findContSum cyph
  where
    findContSum (l:ls)
      | last psums == invalid = minimum terms + maximum terms
      | otherwise             = findContSum ls
      where
        psums = takeWhile (<=invalid) $ scanl (+) l ls
        terms = take (length psums) (l:ls)

    invalid = findInvalid (reverse . take n $ cyph) (drop n cyph)
    
    n = 25
    findInvalid ls (s:ss)
      | hasSum ls s = findInvalid ls' ss
      | otherwise   = s
      where
        ls' = s : take (n-1) ls

    hasSum ls s = s `elem` sums ls


sums [] = []
sums (l:ls) = map (l+) ls ++ sums ls
    
