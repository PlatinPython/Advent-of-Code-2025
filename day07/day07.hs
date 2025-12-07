import Control.Applicative ((<|>))
import Control.Arrow (first, (&&&))
import Criterion.Main (bench, defaultMain, nfIO)
import Data.List (elemIndex, elemIndices)
import Data.Map (Map, adjust, alter, findWithDefault, singleton)
import Data.Maybe (fromJust)

main :: IO ()
main =
  defaultMain
    [ bench "Day 07 Part 1" $ nfIO $ part1 "day07/input.txt",
      bench "Day 07 Part 2" $ nfIO $ part2 "day07/input.txt"
    ]

type Tachyons = Map Integer Int

part1 :: String -> IO Int
part1 file = snd . solve <$> readFile file

part2 :: String -> IO Int
part2 file = sum . fst . solve <$> readFile file

solve :: String -> (Tachyons, Int)
solve = uncurry (foldl moveTachyons) . first ((,0) . flip singleton 1 . fromIntegral . fromJust . elemIndex 'S') . (head &&& tail) . lines

moveTachyons :: (Tachyons, Int) -> String -> (Tachyons, Int)
moveTachyons acc line = case map fromIntegral . elemIndices '^' $ line of
  [] -> acc
  is -> split acc is
  where
    split :: (Tachyons, Int) -> [Integer] -> (Tachyons, Int)
    split acc@(beams, splits) splitters = case filter (\i -> (> 0) . findWithDefault 0 i $ beams) splitters of
      [] -> acc
      is -> (foldl (\b i -> alter (mapMaybe (+ findWithDefault 0 i b)) (i + 1) . alter (mapMaybe (+ findWithDefault 0 i b)) (i - 1) . adjust (subtract . findWithDefault 0 i $ b) i $ b) beams is, splits + length is)
    mapMaybe :: (Int -> Int) -> Maybe Int -> Maybe Int
    mapMaybe f m = fmap f m <|> Just 1
