import Text.Parsec
import qualified Data.HashMap.Strict as M

main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


readD :: String -> [Integer]
readD s = num
  where
    Right num = parse (map read <$> many1 digit `sepEndBy` newline) "" s


solve :: [Integer] -> pInteger
solve pubs = loop cardloop 1 doorpub
  where
    cardpub:doorpub:_ = pubs

    getLoopSize pub = helper 0 1
      where
        helper n val
          | val == pub = n
          | otherwise  = helper (n+1) (loop1 val 7)

    loopSizes = map getLoopSize pubs

    cardloop:_ = loopSizes


loop1 val num = val*num `rem` 20201227
