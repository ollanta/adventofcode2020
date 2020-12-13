import Text.Parsec


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


readD :: String -> (Integer,[Integer])
readD s = busses
  where
    Right busses = parse readBusses "" s

    readBusses = do
      min <- many1 digit
      newline
      busses <- choice [string "x", many1 digit] `sepBy` string ","
      return (read min, map read $ filter (/="x") busses)


solve (start, stops) = calcAns $ minimum (zip nextStop stops)
  where
    nextStop = zipWith (*) stops (map ((+1) . div start) stops)
    calcAns (nxt, stop) = (nxt - start) * stop
