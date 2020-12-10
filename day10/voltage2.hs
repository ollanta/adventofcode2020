import Data.List
import qualified Data.HashMap.Strict as M

main :: IO ()
main = do
  interact (show . solve . map read . lines)
  putStrLn ""


solve :: [Integer] -> Integer
solve volts = ways m svolts
  where
    svolts = sort volts
    m = M.fromList [(0, 1)]


ways :: M.HashMap Integer Integer -> [Integer] -> Integer
ways m (v:vs) = ways m' vs
  where
    conns = sum . map (\d -> M.lookupDefault 0 (v-d) m) $ [1,2,3]
    m' = M.insert v conns m
ways m [] = snd . maximum . M.toList $ m
