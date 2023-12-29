{-# LANGUAGE BangPatterns #-}
import Text.Parsec
import qualified Data.HashMap.Strict as M


main :: IO ()
main = do
  interact (showD . solve . readD)
  putStrLn ""


readD :: String -> [Int]
readD s = map (read . (:[])) $ takeWhile (/='\n') s


showD = show


showMap (c,l,cm) = show (c, l, a*b, nexts)
  where
    nexts@(_:a:b:_) = take 9 . iterate (cm M.!) $ 1


solve cups = showMap . head . drop n_moves . iterate move $ (head morecups, last morecups, cupMap)
  where
    n_moves = 10000000
    n_cups = 1000000

    morecups = take n_cups $ cups ++ [10..]

    cupMap = M.fromList $ zip morecups (tail $ cycle morecups)

    move (!current, previous, cupMap) = (next, current, M.union updateMap cupMap)
      where
        (_:p1:p2:p3:next:_) = iterate (cupMap M.!) $ current

        destNum = head [num |
                        num <- map (`mod` (n_cups+1)) [current-1,current-2..],
                        not (num `elem` [0,p1,p2,p3])]

        destNext = cupMap M.! destNum

        updateMap = M.fromList [(destNum, p1), (p3, destNext), (current, next)]
