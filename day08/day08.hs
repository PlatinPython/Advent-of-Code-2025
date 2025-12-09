import Control.Lens (each, view, _1)
import Control.Lens.Operators ((%~))
import Criterion.Main (bench, defaultMain, nfIO)
import Data.Bifunctor (bimap)
import Data.Either (fromRight)
import Data.List (nub, sortOn, tails)
import Data.Map (Map, elems, fromList, (!))
import Data.Maybe (fromJust)
import Data.Ord (Down (Down))
import Data.UnionFind.IO (Point, descriptor, fresh, repr, union')
import Text.Parsec (char, endBy1, newline, parse)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (int)

main :: IO ()
main =
  defaultMain
    [ bench "Day 08 Part 1" $ nfIO $ part1 1000 "day08/input.txt",
      bench "Day 08 Part 2" $ nfIO $ part2 "day08/input.txt"
    ]

type Junction = (Int, Int, Int)

type Connection = (Junction, Junction)

part1 :: Int -> String -> IO Int
part1 cables file = product . take 3 . sortOn Down . snd <$> solve file (take cables) (const False)

part2 :: String -> IO Int
part2 file = uncurry (*) . (each %~ view _1) . fromJust . fst <$> solve file id ((== 1) . length)

solve :: String -> ([Connection] -> [Connection]) -> ([Point Int] -> Bool) -> IO (Maybe Connection, [Int])
solve file pairTrans cond = do
  junctions <- fromRight [] . parse parseJunctions file <$> readFile file
  junctionMap <- fromList <$> mapM (\j -> (,) j <$> fresh 1) junctions
  let pairs = pairTrans . map snd . sortOn fst . map (\x -> (,x) . uncurry distance $ x) $ [(x, y) | (x : ys) <- tails junctions, y <- ys]
  applyUntil cond junctionMap pairs
  where
    distance :: Junction -> Junction -> Float
    distance (x1, y1, z1) (x2, y2, z2) = sqrt (x' * x' + y' * y' + z' * z')
      where
        x' = fromIntegral $ x2 - x1
        y' = fromIntegral $ y2 - y1
        z' = fromIntegral $ z2 - z1
    applyUntil :: ([Point Int] -> Bool) -> Map Junction (Point Int) -> [Connection] -> IO (Maybe Connection, [Int])
    applyUntil cond jm = go Nothing
      where
        go :: Maybe Connection -> [Connection] -> IO (Maybe Connection, [Int])
        go lastPair [] = do
          rs <- fmap nub . mapM repr . elems $ jm
          (lastPair,) <$> mapM descriptor rs
        go _ (jp : jps) = do
          uncurry union' (bimap (jm !) (jm !) jp) $ ((.) . (.)) return (+)
          rs <- fmap nub . mapM repr . elems $ jm
          if cond rs
            then
              (Just jp,) <$> mapM descriptor rs
            else
              go (Just jp) jps

parseJunctions :: Parser [Junction]
parseJunctions = flip endBy1 newline $ do
  x <- int
  char ','
  y <- int
  char ','
  z <- int
  return (x, y, z)
