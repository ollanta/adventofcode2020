import Text.Parsec
import qualified Data.HashSet as S


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


readD :: String -> [[Int]]
readD s = decks
  where
    Right decks = parse (readPlayer `sepEndBy` newline) "" s

    readPlayer = do
      manyTill anyChar newline
      cards <- (many1 digit) `sepEndBy` newline
      return $ (map read cards)


solve [p1deck,p2deck] = score $ play S.empty (p1deck, p2deck)
  where
    play :: S.HashSet ([Int], [Int]) -> ([Int], [Int]) -> ([Int], [Int])
    play seen decks@((a:as), (b:bs))
      | decks `S.member` seen = (a:as, [])
      | a <= length as && b <= length bs = case play S.empty (take a as, take b bs) of
          (_, []) -> aWin
          ([], _) -> bWin
      | a > b = aWin
      | a < b = bWin
      where
        aWin = play seen' (as++[a,b], bs)
        bWin = play seen' (as, bs++[b,a])
        seen' = S.insert decks seen

    play _ decks = decks

    score decks = sum $ zipWith (*) [1..] (reverse . winner $ decks)
      where
        winner ([],deck) = deck
        winner (deck,[]) = deck
