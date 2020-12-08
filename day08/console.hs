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


solve prog = runProgram progA
  where
    progA = A.listArray (0, length prog - 1) prog


runProgram progA = run 0 0 runCount
  where
    pbounds = A.bounds progA
    runCount = A.listArray pbounds (repeat 0)

    run i acc rcs
      | rc > 0 = acc
      | otherwise = run i' acc' rcs'
      where
        (op, n) = progA A.! i
        rc = rcs A.! i
        rcs' = rcs A.// [(i, rc+1)]

        (i', acc') = case op of
          "nop" -> (i+1, acc)
          "acc" -> (i+1, acc + n)
          "jmp" -> (i + n, acc)
