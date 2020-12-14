import Text.Parsec
import qualified Data.HashMap.Strict as M


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


data Op = Mask String | Write Integer Integer
  deriving (Eq, Show)


readD :: String -> [Op]
readD s = ops
  where
    Right ops = parse (choice [try readMask, readOp] `endBy` newline) "" s

    readMask = do
      string "mask = "
      mask <- many1 (oneOf "X10")
      return $ Mask mask
    readOp = do
      string "mem["
      loc <- many1 digit
      string "] = "
      val <- many1 digit
      return $ Write (read loc) (read val)
      

solve ops = calcSum pairedWrites
  where
    initMask = repeat 'X'

    -- this list is reversed
    pairedWrites = snd $ foldl pairOp (initMask, []) ops

    pairOp (mask, ops) (Mask newmask) = (newmask, ops)
    pairOp (mask, ops) (Write loc val) = (mask, (maskedLoc, val):ops)
      where
        maskedLoc = maskLoc mask loc


    maskLoc :: String -> Integer -> String
    maskLoc mask loc = zipWith maskBit mask (toBits loc)
      where
        maskBit '0' 1 = '1'
        maskBit '0' 0 = '0'
        maskBit c   _ = c


toBits :: Integer -> [Integer]
toBits v = helper [35,34..0] v
  where
    helper :: [Integer] -> Integer -> [Integer]
    helper []         v = []
    helper (bit:bits) v = vbit : helper bits v'
      where
        (vbit, v') = v `divMod` (2^bit)


calcSum :: [(String, Integer)] -> Integer
calcSum lvs = helper [] lvs
  where
    helper _ [] = 0
    helper pastLocs ((loc, val):lvs) = val * writes + helper (loc:pastLocs) lvs
      where
        writes = calcWrites loc pastLocs


calcWrites loc plocs = sum . map calcMult $ rlocs
  where
    calcMult = (2^) . length . filter (=='X')
    rlocs = reduceLoc [loc] plocs


reduceLoc :: [String] -> [String] -> [String]
reduceLoc locs [] = locs
reduceLoc locs (pl:pls) = reduceLoc locs' pls
  where
    locs' = concatMap (disjointFrom pl) locs

    disjointFrom :: String -> String -> [String]
    disjointFrom pl loc = genD "" [] pl loc
      where
        genD :: String -> [String] -> String -> String -> [String]
        genD rpref acc [] [] = acc
        genD rpref acc (b:bs) (a:as)
          -- if a_i is X and b_i is V, replacing a_i with !V is disjoint, a_i=v needs further recursion
          | a == 'X' && b /= 'X' = genD (b:rpref) (newacc : acc) bs as
          -- if there's no overlap, we short-circuit and return the original loc
          | a /= b && b /= 'X' = [loc]
          | otherwise = genD (a:rpref) acc bs as
          where
            newacc = reverse rpref ++ flip b : as


    flip '0' = '1'
    flip '1' = '0'
