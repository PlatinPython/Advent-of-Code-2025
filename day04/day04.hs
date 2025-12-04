import Control.Monad (filterM)
import Criterion.Main (bench, defaultMain, nfIO)
import Data.Array.IO (IOArray, getBounds, newArray, readArray, writeArray)
import Data.List (delete)
import Data.Set (Set, difference, fromList, toList)

main :: IO ()
main =
  defaultMain
    [ bench "Day 04 Part 1" $ nfIO $ part1 "day04/input.txt",
      bench "Day 04 Part 2" $ nfIO $ part2 "day04/input.txt"
    ]

type Pos = (Int, Int)

type Array = IOArray Pos Char

part1 :: String -> IO Int
part1 file = uncurry solve =<< createArray . lines =<< readFile file
  where
    solve :: Array -> Set Pos -> IO Int
    solve arr posToCheck = do
      (xSize, ySize) <- snd <$> getBounds arr
      length <$> accessible arr posToCheck

part2 :: String -> IO Int
part2 file = uncurry solve =<< createArray . lines =<< readFile file
  where
    solve :: Array -> Set Pos -> IO Int
    solve arr posToCheck = do
      pos <- accessible arr posToCheck
      mapM_ (flip (writeArray arr) '.') pos
      let moved = length pos
      if moved == 0 then return 0 else (+ moved) <$> solve arr (difference posToCheck $ fromList pos)

createArray :: [String] -> IO (Array, Set Pos)
createArray map = do
  let bounds@(xSize, ySize) = (length (head map) + 1, length map + 1)
  arr <- newArray ((0, 0), bounds) '.' :: IO Array
  let posToCheck = [(x + 1, y + 1) | x <- [0 .. xSize - 2], y <- [0 .. ySize - 2], (map !! y) !! x == '@']
  mapM_ (flip (writeArray arr) '@') posToCheck
  return (arr, fromList posToCheck)

accessible :: Array -> Set Pos -> IO [Pos]
accessible arr toCheck = do
  filterM (check arr) $ toList toCheck
  where
    check :: Array -> Pos -> IO Bool
    check arr pos = do
      c <- readArray arr pos
      l <- mapM (readArray arr) . neighbors $ pos
      return . (&& c == '@') . (< 4) . length . filter ('@' ==) $ l

neighbors :: Pos -> [Pos]
neighbors (x, y) = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]
