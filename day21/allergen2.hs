import Text.Parsec
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.List


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


readD :: String -> [([String], [String])]
readD s = foods
  where
    Right foods = parse (readFood `sepEndBy` newline) "" s

    readFood = do
      ingredients <- (many1 letter) `sepEndBy` string " "
      string "(contains "
      allergens <- (many1 letter) `sepBy` string ", "
      string ")"
      return $ (ingredients, allergens)


solve foods = intercalate "," . map snd . sort . M.toList . reduceMap $ allergenMap
  where
    allergenMap = M.fromListWith S.intersection [(allergen, S.fromList ingredients) |
                                                 (ingredients, allergens) <- foods,
                                                 allergen <- allergens]

    reduceMap am
      | M.size am == S.size determined = M.map (head . S.toList) am
      | otherwise = reduceMap am'
      where
        determined = M.foldl' S.union S.empty . M.filter ((==1) . S.size) $ am
        am' = M.map removeDetermined am

        removeDetermined ingredients
          | S.size ingredients == 1 = ingredients
          | otherwise               = S.difference ingredients determined
