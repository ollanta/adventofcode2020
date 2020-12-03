import Data.List

main :: IO ()
main = interact run
  where run = show . solve . map read . lines

solve :: [Integer] -> Integer
solve l = solve' . map (\(a,b,c) -> (a+b+c, a*b*c)) $ gen3 l
  where
    solve' ((2020, ans):_) = ans
    solve' (_:ls) = solve' ls

gen3 :: [Integer] -> [(Integer, Integer, Integer)]
gen3 (l:ls) = zipWith (\a (b,c) -> (a,b,c)) (repeat l) (gen ls) ++ gen3 ls
gen3 []      = []

gen :: [Integer] -> [(Integer, Integer)]
gen (l:ls) = zip (repeat l) ls ++ gen ls
gen []     = []
