import Text.Parsec


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


readD :: String -> (Integer,[(Integer, Integer)])
readD s = busses
  where
    Right busses = parse readBusses "" s

    readBusses = do
      min <- many1 digit
      newline
      busses <- choice [string "x", many1 digit] `sepBy` string ","
      return (read min, map (\(i,x) -> (i, read x)) . filter ((/="x") . snd) . zip [1..] $ busses)


solve (start, stops) = foldl calcMOF (0,d0,k0) (tail stops)
  where
    (d0, k0) = head stops

    calcMOF (d, d0, k0) (d1,k1) = (d', d1, k0*k1)
      where
        d' = getRem k0 k1 (d1 - d0 + d)

getRem k0 k1 rem
  | k0 < k1 = head [ i * k1 | i <- [1..k0], i*k1 `mod` k0 == rem]
  | k1 < k0 = getRem k1 k0 (k1-(rem `mod` k1)) + rem
