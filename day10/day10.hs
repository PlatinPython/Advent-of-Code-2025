import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Criterion.Main (bench, defaultMain, nfIO)
import Data.Bits (setBit, xor)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (minimumBy, subsequences)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.SBV (OptimizeResult (LexicographicResult), OptimizeStyle (Lexicographic), constrain, getModelValue, minimize, optimize, sInteger, (.==), (.>=))
import Text.Parsec (between, char, endBy1, many1, parse, sepBy1, space, try, (<|>))
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Number (int)

main :: IO ()
main =
  defaultMain
    [ bench "Day 10 Part 1" $ nfIO $ part1 "day10/input.txt",
      bench "Day 10 Part 2" $ nfIO $ part2 "day10/input.txt"
    ]

type Lights = Int

type Button = [Int]

type Joltages = [Int]

type Machine = (Lights, [Button], Joltages)

part1 :: String -> IO Int
part1 file = sum . map (\(l, bs, _) -> (snd . minimumBy (comparing snd) . filter ((== l) . fst) . map (foldl xor 0 &&& length) . subsequences . map (foldl setBit 0)) bs) . fromRight [] . mapM (parse parseMachine file) . lines <$> readFile file

part2 :: String -> IO Integer
part2 file = do
  machines <- fromRight [] . mapM (parse parseMachine file) . lines <$> readFile file
  sum . catMaybes <$> mapM (\(_, bs, js) -> solve bs js) machines
  where
    solve :: [Button] -> Joltages -> IO (Maybe Integer)
    solve buttons joltages = do
      res <- optimize Lexicographic $ do
        xs <- mapM sInteger ["x" ++ show i | i <- [1 .. length buttons]]
        mapM_ (\x -> constrain $ x .>= 0) xs
        forM_ [0 .. length joltages - 1] $ \j ->
          constrain $ sum [x | (btn, x) <- zip buttons xs, j `elem` btn] .== fromIntegral (joltages !! j)
        minimize "presses" (sum xs)
      return $ case res of
        LexicographicResult res -> getModelValue "presses" res
        _ -> Nothing

parseMachine :: Parser Machine
parseMachine = do
  lights <- parseLights
  space
  buttons <- parseButton `endBy1` space
  joltages <- parseJoltages
  return (lights, buttons, joltages)

parseLights :: Parser Lights
parseLights = between (char '[') (char ']') $ do
  lights <- many1 $ try (char '.' $> False) <|> try (char '#' $> True)
  return . foldr (\b acc -> 2 * acc + fromEnum b) 0 $ lights

parseButton :: Parser Button
parseButton = between (char '(') (char ')') $ int `sepBy1` char ','

parseJoltages :: Parser Joltages
parseJoltages = between (char '{') (char '}') $ int `sepBy1` char ','
