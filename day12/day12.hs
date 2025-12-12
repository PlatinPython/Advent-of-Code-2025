import Control.Arrow (second)
import Criterion.Main (bench, defaultMain, nfIO)
import Data.Functor (($>))
import Text.Parsec (char, count, digit, endBy1, newline, optional, parse, string, try, (<|>))
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (int)

main :: IO ()
main = defaultMain [bench "Day 12 Part 1" $ nfIO $ part1 "day12/input.txt"]

part1 :: String -> IO Int
part1 file = do
  Right (shapes, regions) <- parse parseInput file <$> readFile file
  return . sum . map (fromEnum . uncurry (>) . second (sum . zipWith (*) shapes)) $ regions

parseInput :: Parser ([Int], [(Int, [Int])])
parseInput = do
  shapes <- count 6 $ do
    shape <- parseShape
    newline
    return shape
  regions <- parseRegion `endBy1` newline
  return (shapes, regions)

parseShape :: Parser Int
parseShape = do
  digit
  char ':'
  newline
  fmap sum . count 3 $ do
    tiles <- count 3 $ try (char '.' $> 0) <|> try (char '#' $> 1)
    newline
    return $ sum tiles

parseRegion :: Parser (Int, [Int])
parseRegion = do
  x <- int
  char 'x'
  y <- int
  string ": "
  shapes <- count 6 (int <* optional (char ' '))
  return (x * y, shapes)
