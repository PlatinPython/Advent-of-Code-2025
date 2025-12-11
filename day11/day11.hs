import Control.Lens (view, _1, _3)
import Criterion.Main (bench, defaultMain, nfIO)
import Data.Array ((!))
import Data.Either (fromRight)
import Data.Graph (Graph, Vertex, graphFromEdges, topSort)
import Data.Map (Map, findWithDefault, insertWith, singleton)
import Data.Maybe (fromJust)
import Text.Parsec (anyChar, count, parse, sepBy1, space, string)
import Text.Parsec.String (Parser)

main :: IO ()
main = do
  defaultMain
    [ bench "Day 11 Part 1" $ nfIO $ part1 "day11/input.txt",
      bench "Day 11 Part 2" $ nfIO $ part2 "day11/input.txt"
    ]

part1 :: String -> IO Int
part1 file = do
  (graph, vertexFromKey) <- constructGraph file
  let you = fromJust $ vertexFromKey "you"
      out = fromJust $ vertexFromKey "out"
  return $ countPaths graph you out

part2 :: String -> IO Int
part2 file = do
  (graph, vertexFromKey) <- constructGraph file
  let svr = fromJust $ vertexFromKey "svr"
      dac = fromJust $ vertexFromKey "dac"
      fft = fromJust $ vertexFromKey "fft"
      out = fromJust $ vertexFromKey "out"
  return $ countPathsThrough graph dac fft svr out

constructGraph :: String -> IO (Graph, String -> Maybe Vertex)
constructGraph file = liftA2 (,) (view _1) (view _3) . graphFromEdges . (("out", "out", []) :) . fromRight [] . mapM (parse parseCables file) . lines <$> readFile file

countPaths :: Graph -> Vertex -> Vertex -> Int
countPaths g src dst =
  let order = topSort g
      pathsMap = foldl update (singleton src 1) order
   in findWithDefault 0 dst pathsMap
  where
    update :: Map Vertex Int -> Vertex -> Map Vertex Int
    update acc v =
      let paths = findWithDefault 0 v acc
       in foldl (\m u -> insertWith (+) u paths m) acc (g ! v)

countPathsThrough :: Graph -> Vertex -> Vertex -> Vertex -> Vertex -> Int
countPathsThrough g a b src dst =
  let pathsSrcToA = countPaths g src a
      pathsAToB = countPaths g a b
      pathsBToDst = countPaths g b dst
      pathsSrcToB = countPaths g src b
      pathsBToA = countPaths g b a
      pathsAToDst = countPaths g a dst
   in pathsSrcToA * pathsAToB * pathsBToDst + pathsSrcToB * pathsBToA * pathsAToDst

parseCables :: Parser (String, String, [String])
parseCables = do
  from <- count 3 anyChar
  string ": "
  to <- count 3 anyChar `sepBy1` space
  return (from, from, to)
