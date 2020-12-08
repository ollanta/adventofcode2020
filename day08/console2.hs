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


solve prog = filter (/=Nothing) . map (run 0 0 runCount) $ progAs
  where
    l = length prog - 1
    progA = A.listArray (0, l) prog
    runCount = A.listArray (0, l) (repeat 0)

    progAs = [
      progA A.// [(i, (flip op, dir, n))] |
      (i, (op, dir, n)) <- A.assocs progA,
      op == "nop" || op == "jmp"
                                          ]
    flip "nop" = "jmp"
    flip "jmp" = "nop"
      

    run i acc rcs prg
      | i > l  = Just acc
      | rc > 0 = Nothing
      | otherwise = run i' acc' (rcs A.// [(i, rc+1)]) prg
      where
        (op, dir, n) = prg A.! i
        rc = rcs A.! i

        add a '+' b = a+b
        add a '-' b = a-b

        (i', acc') = case op of
          "nop" -> (i+1, acc)
          "acc" -> (i+1, add acc dir n)
          "jmp" -> (add i dir n, acc)
