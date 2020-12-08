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


solve prog = filter (/=Nothing) . map runProgram $ progAs
  where
    l = length prog - 1
    progA = A.listArray (0, l) prog

    progAs = [progA A.// [(i, (flip op, n))] |
              (i, (op, n)) <- A.assocs progA,
              op == "nop" || op == "jmp"]

    flip "nop" = "jmp"
    flip "jmp" = "nop"


runProgram progA = run 0 0 runCount
  where
    pbounds = A.bounds progA
    runCount = A.listArray pbounds (repeat 0)

    l = snd pbounds

    run i acc rcs
      | i > l  = Just acc
      | rc > 0 = Nothing
      | otherwise = run i' acc' rcs'
      where
        (op, n) = progA A.! i
        rc = rcs A.! i
        rcs' = rcs A.// [(i, rc+1)]

        (i', acc') = case op of
          "nop" -> (i+1, acc)
          "acc" -> (i+1, acc + n)
          "jmp" -> (i + n, acc)
