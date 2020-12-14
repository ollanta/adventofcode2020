import Text.Parsec
import qualified Data.HashMap.Strict as M


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


data Op = Mask String | Write Integer Integer


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
      

solve ops = sum . M.elems . snd $ foldl runOp (initMask, M.empty) ops
  where
    initMask = toMask . take 36 $ repeat 'X'

    runOp (mask, mem) (Mask newmask) = (toMask newmask, mem)
    runOp (mask, mem) (Write loc val) = (mask, M.insert loc maskedVal mem)
      where
        maskedVal = maskVal mask val

    toMask :: String -> [(Integer, Integer)]
    toMask strMask = map (\(b,v) -> (b, read (v:[]))) . filter ((/='X') . snd) . zip [35,34..0] $ strMask

    maskVal ((bit,mv):mask) val
      | valbit == mv = maskVal mask val
      | otherwise    = maskVal mask (val + (mv - valbit) * 2^bit)
      where
        valbit = case val `mod` 2^(bit+1) == val `mod` 2^(bit) of
          True -> 0
          False -> 1
    maskVal [] val = val
