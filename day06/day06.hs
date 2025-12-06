import Criterion.Main (bench, defaultMain, nfIO)
import Data.Char (isSpace)
import Data.List (transpose)
import Data.List.Split (splitWhen)

main :: IO ()
main =
  defaultMain
    [ bench "Day 06 Part 1" $ nfIO $ part1 "day06/input.txt",
      bench "Day 06 Part 2" $ nfIO $ part2 "day06/input.txt"
    ]

part1 :: String -> IO Int
part1 file = sum . map (solve . reverse) . transpose . map words . lines <$> readFile file
  where
    solve :: [String] -> Int
    solve ("+" : xs) = sum . map read $ xs
    solve ("*" : xs) = product . map read $ xs

-- part2 :: String -> IO Int
part2 file = sum . map solve . splitWhen null . map (filter (not . isSpace)) . transpose . lines <$> readFile file
  where
    solve :: [String] -> Int
    solve (x : xs)
      | last x == '+' = sum . map read $ (init x : xs)
      | last x == '*' = product . map read $ (init x : xs)
