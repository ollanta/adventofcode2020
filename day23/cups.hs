import Text.Parsec
import qualified Data.HashMap.Strict as M
import Data.Either

main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


readD :: String -> [Integer]
readD s = map (read . (:[])) $ takeWhile (/='\n') s


solve cups = head . drop 100 . map rotate . iterate move $ cups
  where
    move (c:cs) = pre ++ [dest] ++ pickedUp ++ after ++ [c]
      where
        (pickedUp, rest) = splitAt 3 cs
        destNum = head [num' |
                        num <- [c-1,c-2..],
                        let num' = num `mod` 10,
                        num' /= 0,
                        not (num' `elem` pickedUp)]

        (pre, dest:after) = break (==destNum) rest

    rotate cs = start ++ end
      where
        (end, start) = break (==1) cs
