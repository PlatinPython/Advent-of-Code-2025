import Criterion.Main (bench, defaultMain, nfIO)
import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

main :: IO ()
main =
  defaultMain
    [ bench "Day 03 Part 1" $ nfIO $ part1 "day03/input.txt",
      bench "Day 03 Part 2" $ nfIO $ part2 "day03/input.txt"
    ]

part1 :: String -> IO Int
part1 = solve 2

part2 :: String -> IO Int
part2 = solve 12

solve :: Int -> String -> IO Int
solve k file = sum . map (foldl ((+) . (* 10)) 0 . largestNumber k . map digitToInt) . lines <$> readFile file

largestNumber :: Int -> [Int] -> [Int]
largestNumber 0 _ = []
largestNumber _ [] = []
largestNumber k xs =
  let n = length xs
      (init, _) = splitAt (n - k + 1) xs
      best = maximum init
      i = fromJust $ elemIndex best init
   in best : largestNumber (k - 1) (drop (i + 1) xs)
