import Text.Parsec
import qualified Data.Array as A


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""

readD :: String -> [(String, Int)]
readD s = program
  where
    Right program = parse (readLine `endBy` newline) "" s

    signedNum = fmap read (posNum <|> negNum)
      where
        posNum = string "+" *> many1 digit
        negNum = string "-" <> many1 digit

    readLine = do
      op <- many1 letter
      space
      num <- signedNum
      return (op, num)


solve prog = run 0 0 runCount
  where
    progA = A.listArray (0, length prog - 1) prog
    runCount = A.listArray (0, length prog -1) (repeat 0)

    run i acc rcs
      | rc > 0 = acc
      | otherwise = run i' acc' (rcs A.// [(i, rc+1)])
      where
        (op, n) = progA A.! i
        rc = rcs A.! i

        (i', acc') = case op of
          "nop" -> (i+1, acc)
          "acc" -> (i+1, acc + n)
          "jmp" -> (i + n, acc)
