import Control.Monad (guard)
import Criterion.Main (bench, defaultMain, nfIO)
import Data.Either (fromRight)
import Data.List (nub)
import Text.Parsec (char, parse, sepBy1)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (int)

main :: IO ()
main =
  defaultMain
    [ bench "Day 02 Part 1" $ nfIO $ part1 "day02/input.txt",
      bench "Day 02 Part 2" $ nfIO $ part2 "day02/input.txt"
    ]

part1 :: String -> IO Int
part1 file = sum . concatMap findInvalid . fromRight [] . parse parseRanges file <$> readFile file
  where
    findInvalid :: (Int, Int) -> [Int]
    findInvalid range@(start, end) = do
      k <- filter (/= 0) [blockSize start .. blockSize end]
      let factor = 10 ^ k + 1
      x <- uncurry enumFromTo $ blockBounds range factor k
      return $ x * factor
    blockSize :: Int -> Int
    blockSize = (`div` 2) . numDigits

part2 :: String -> IO Int
part2 file = sum . concatMap (nub . findInvalid) . fromRight [] . parse parseRanges file <$> readFile file
  where
    findInvalid :: (Int, Int) -> [Int]
    findInvalid range@(start, end) = do
      d <- [numDigits start .. numDigits end]
      k <- divisors d
      let m = 10 ^ k
      let factor = (m ^ (d `div` k) - 1) `div` (m - 1)
      guard $ factor /= 1
      x <- uncurry enumFromTo $ blockBounds range factor k
      return $ x * factor
    divisors :: Int -> [Int]
    divisors n = [x | x <- [1 .. n], n `mod` x == 0]

numDigits :: Int -> Int
numDigits = (+ 1) . floor . logBase 10 . fromIntegral

blockBounds :: (Int, Int) -> Int -> Int -> (Int, Int)
blockBounds (start, end) f k =
  let lower = max (10 ^ (k - 1)) $ (start + f - 1) `div` f
      upper = min (10 ^ k - 1) $ end `div` f
   in (lower, upper)

parseRanges :: Parser [(Int, Int)]
parseRanges = sepBy1 parseRange (char ',')

parseRange :: Parser (Int, Int)
parseRange = do
  start <- int
  _ <- char '-'
  end <- int
  return (start, end)
