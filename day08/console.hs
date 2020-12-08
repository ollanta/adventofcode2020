import Text.Parsec
import qualified Data.Array as A


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""

readD :: String -> [(String, Char, Int)]
readD s = program
  where
    Right program = parse (readLine `endBy` newline) "" s

    readLine = do
      op <- many1 letter
      space
      dir <- oneOf "+-"
      num <- many1 digit
      return (op, dir, read num)


solve prog = run 0 0 runCount
  where
    progA = A.listArray (0, length prog - 1) prog
    runCount = A.listArray (0, length prog -1) (repeat 0)

    run i acc rcs
      | rc > 0 = acc
      | otherwise = run i' acc' (rcs A.// [(i, rc+1)])
      where
        (op, dir, n) = progA A.! i
        rc = rcs A.! i

        add a '+' b = a+b
        add a '-' b = a-b

        (i', acc') = case op of
          "nop" -> (i+1, acc)
          "acc" -> (i+1, add acc dir n)
          "jmp" -> (add i dir n, acc)
