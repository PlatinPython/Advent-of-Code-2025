import Criterion.Main (bench, defaultMain, nfIO)
import Data.Either (fromRight)
import Data.List (sortBy)
import Data.Ord (comparing)
import Text.Parsec (char, endBy, newline, parse)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (int)

main :: IO ()
main =
  defaultMain
    [ bench "Day 05 Part 1" $ nfIO $ part1 "day05/input.txt",
      bench "Day 05 Part 2" $ nfIO $ part2 "day05/input.txt"
    ]

part1 :: String -> IO Int
part1 file = do
  (ranges, ingredients) <- fromRight ([], []) . parse parseDatabase file <$> readFile file
  return . length . filter (\x -> any (\(s, e) -> s <= x && x <= e) ranges) $ ingredients

part2 :: String -> IO Int
part2 file = sum . map ((+ 1) . uncurry (flip (-))) . foldr merge [] . sortBy (comparing fst) . fst . fromRight ([], []) . parse parseDatabase file <$> readFile file
  where
    merge :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    merge (s, e) [] = [(s, e)]
    merge (s, e) ((s', e') : rs)
      | e >= s' - 1 = (s, max e e') : rs
      | otherwise = (s, e) : (s', e') : rs

parseDatabase :: Parser ([(Int, Int)], [Int])
parseDatabase = do
  ranges <- parseRange `endBy` newline
  newline
  ingredient <- int `endBy` newline
  return (ranges, ingredient)

parseRange :: Parser (Int, Int)
parseRange = do
  start <- int
  char '-'
  end <- int
  return (start, end)
