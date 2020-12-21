import Text.Parsec
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S


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


solve foods = sum . map countOccurrences . S.toList $ safeIngredients
  where
    allergenMap = M.fromListWith S.intersection [(allergen, S.fromList ingredients) |
                                                 (ingredients, allergens) <- foods,
                                                 allergen <- allergens]

    allIngredients = foldl1 S.union . map (S.fromList . fst) $ foods

    safeIngredients = S.difference allIngredients (foldl1 S.union $ M.elems allergenMap)

    countOccurrences ingr = length . filter (elem ingr . fst) $ foods
