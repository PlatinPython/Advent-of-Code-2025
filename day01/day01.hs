import Criterion.Main (bench, defaultMain, nfIO)
import Data.Bifunctor (Bifunctor (first))

main :: IO ()
main =
  defaultMain
    [ bench "Day 01 Part 1" $ nfIO $ part1 "day01/input.txt",
      bench "Day 01 Part 2" $ nfIO $ part2 "day01/input.txt"
    ]

part1 :: String -> IO Int
part1 file = length . filter (== 0) . scanl rotate 50 . map parse . lines <$> readFile file
  where
    rotate :: Int -> Int -> Int
    rotate pos x = (pos + x) `mod` 100

part2 :: String -> IO Int
part2 file = fst . foldl rotate (0, 50) . map parse . lines <$> readFile file
  where
    rotate :: (Int, Int) -> Int -> (Int, Int)
    rotate (clicks, pos) x = first ((+ clicks) . abs) $ (pos + x) `divMod` 100

parse :: String -> Int
parse ('R' : x) = read x
parse ('L' : x) = negate . read $ x
