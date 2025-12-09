import Control.Lens (view, _3)
import Criterion.Main (bench, defaultMain, nfIO)
import Data.Either (fromRight)
import Data.List (sortOn, tails)
import Data.Ord (Down (Down))
import Text.Parsec (char, endBy1, newline, parse)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (int)

main :: IO ()
main =
  defaultMain
    [ bench "Day 09 Part 1" $ nfIO $ part1 "day09/input.txt",
      bench "Day 09 Part 2" $ nfIO $ part2 "day09/input.txt"
    ]

type Tile = (Int, Int)

part1 :: String -> IO Int
part1 file = do
  tiles <- fromRight [] . parse parseTiles file <$> readFile file
  return . maximum . map (uncurry area) $ [(a, b) | (a : bs) <- tails tiles, b <- bs]

part2 :: String -> IO Int
part2 file = do
  tiles <- fromRight [] . parse parseTiles file <$> readFile file
  let edges = zip tiles . tail . cycle $ tiles
  let candidates = map (\(a@(ax, ay), b@(bx, by)) -> ((min ax bx, min ay by), (max ax bx, max ay by), area a b)) $ [(a, b) | (a : bs) <- tails tiles, b <- bs]
  return . view _3 . head . filter (\(a, b, _) -> all (check (a, b)) edges) . sortOn (Down . view _3) $ candidates
  where
    check :: (Tile, Tile) -> (Tile, Tile) -> Bool
    check ((rax, ray), (rbx, rby)) ((eax, eay), (ebx, eby)) = do
      let left = max rax rbx <= min eax ebx
      let right = min rax rbx >= max eax ebx
      let above = max ray rby <= min eay eby
      let below = min ray rby >= max eay eby
      left || right || above || below

area :: Tile -> Tile -> Int
area (x, y) (x', y') = (abs (x - x') + 1) * (abs (y - y') + 1)

parseTiles :: Parser [Tile]
parseTiles = flip endBy1 newline $ do
  x <- int
  char ','
  y <- int
  return (x, y)
