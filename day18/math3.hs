import Text.Parsec
import Text.Parsec.Expr


main :: IO ()
main = do
  interact (show . (\s -> (solve1 s, solve2 s)))
  putStrLn ""


solve1 :: String -> Integer
solve1 s = solveWithTable [[plusOp, multOp]] s


solve2 :: String -> Integer
solve2 s = solveWithTable [[plusOp], [multOp]] s


solveWithTable table s = sum rs
  where
    Right rs = parse (expr `endBy` newline) "" (filter (/=' ') s)

    expr = buildExpressionParser table term

    term = between (string "(") (string ")") expr
       <|> integer

    integer = read <$> many1 digit

multOp = Infix (string "*" >> pure (*)) AssocLeft

plusOp = Infix (string "+" >> pure (+)) AssocLeft
