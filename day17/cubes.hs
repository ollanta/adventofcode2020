import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


type Coord = (Integer,Integer,Integer)


cplus (x0,y0,z0) (x1,y1,z1) = (x0+x1, y0+y1, z0+z1)


readD :: String -> [Coord]
readD s = actives
  where
    rows = lines s
    actives = [(x,y,0) |
               (y,row) <- zip [0..] rows,
               (x,c) <- zip [0..] row,
               c /= '.']
      

solve coords = take 7 $ map S.size (iterate cycle initS)
  where
    initS = S.fromList coords

    cycle coordS = S.union (M.keysSet newActiveM) remainingActive
      where
        remainingActive = S.filter remainActive coordS

        remainActive c = activeNeighbours == 2 || activeNeighbours == 3
          where
            activeNeighbours = countNeighbours coordS c

        coordL = S.toList coordS
        newActiveM = M.filter (==3) $ foldl (\m c -> M.insertWith (+) c 1 m) M.empty (concatMap genNeighbours coordL)

countNeighbours set coord = length . filter (`S.member` set) $ genNeighbours coord

genNeighbours coord = [cplus coord (x,y,z) |
                        x <- [-1..1],
                        y <- [-1..1],
                        z <- [-1..1],
                        (x,y,z) /= (0,0,0)]
