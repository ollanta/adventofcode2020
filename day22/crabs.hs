import Text.Parsec


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


readD :: String -> [[Integer]]
readD s = decks
  where
    Right decks = parse (readPlayer `sepEndBy` newline) "" s

    readPlayer = do
      manyTill anyChar newline
      cards <- (many1 digit) `sepEndBy` newline
      return $ (map read cards)


solve [p1deck,p2deck] = score $ play p1deck p2deck
  where
    play [] bs = bs
    play as [] = as
    play (a:as) (b:bs)
      | a > b = play (as++[a,b]) bs
      | a < b = play as (bs++[b,a])

    score deck = sum $ zipWith (*) [1..] (reverse deck)
