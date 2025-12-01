import Data.Bifunctor (Bifunctor (first))

part1 :: String -> IO Int
part1 file = length . filter (== 0) . scanl rotate 50 . map parse . lines <$> readFile file
  where
    rotate :: Int -> Int -> Int
    rotate pos x = (pos + x) `mod` 100

part2 :: String -> IO Int
part2 file = snd . foldl rotate (0, 50) . map parse . lines <$> readFile file
  where
    rotate :: (Int, Int) -> Int -> (Int, Int)
    rotate (clicks, pos) x = first ((+clicks) . abs) $ (pos + x) `divMod` 100

parse :: String -> Int
parse ('R' : x) = read x
parse ('L' : x) = negate . read $ x
