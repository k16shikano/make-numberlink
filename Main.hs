module Main where

import System.Environment (getArgs)
import System.Random
import Data.Map as M
import Data.List as L

-- take arbitrary number of pairs from
s = [(0,1), (1,2),
     (3,4), (4,5),
     (6,7), (7,8),
     (0,3), (1,4), (2,5),
     (3,6), (4,7), (5,8)]

-- take zero or one pair from each
c1 = [(0,4), (1,3)]
c2 = [(1,5), (2,4)]
c3 = [(3,7), (4,6)]
c4 = [(4,8), (5,7)]

takeWith :: RandomGen g => [(Int, Int)] -> [Bool] -> g -> ([(Int, Int)], g)
takeWith l preds g = (L.map snd $ L.filter fst (L.zip shuffled l), g')
  where (shuffled, g') = fisherYates g preds

takeHV n g = takeWith s (replicate (12 - n) False ++ L.replicate n True) g

takeUptoOne :: RandomGen g => [(Int, Int)] -> g -> ([(Int, Int)], g)
takeUptoOne l g = case randomR (1 :: Int, 3 :: Int) g of
                    (1,g') -> takeWith l [True, False] g'
                    (2,g') -> takeWith l [False, True] g'
                    (3,g') -> takeWith l [False, False] g'

flatten :: [(Int, Int)] -> [Int]
flatten l = concatMap (\p -> [fst p, snd p]) l

complement :: [[Int]] -> [Int]
complement s = L.map (\n -> count n) [0..8]
  where count n = case find (\cs -> n `elem` cs) s of
           Just is -> length is
           Nothing -> 0

sumUpSame l = complement $ group $ sort $ flatten l

showTikz l =
  "\\documentclass[a4, lualatex]{jlreq}" ++
  "\\usepackage{tikz}" ++
  "\\begin{document}" ++
  "\\begin{tikzpicture}[every node/.style={minimum size=3cm,draw,circle,font=\\Huge,align=center}]" ++
  "\\draw (0,0) node {" ++ (show $ l !! 0) ++ "};" ++
  "\\draw (5,0) node {" ++ (show $ l !! 1) ++ "};" ++
  "\\draw (10,0) node {" ++ (show $ l !! 2) ++ "};" ++
  "\\draw (0,5) node {" ++ (show $ l !! 3) ++ "};" ++
  "\\draw (5,5) node {" ++ (show $ l !! 4) ++ "};" ++
  "\\draw (10,5) node {" ++ (show $ l !! 5) ++ "};" ++
  "\\draw (0,10) node {" ++ (show $ l !! 6) ++ "};" ++
  "\\draw (5,10) node {" ++ (show $ l !! 7) ++ "};" ++
  "\\draw (10,10) node {" ++ (show $ l !! 8) ++ "};" ++
  "\\end{tikzpicture}" ++
  "\\end{document}"

main :: IO ()
main = do
  s:s':_ <- fmap (L.map read) getArgs
  setStdGen (mkStdGen s)
  m <- getStdRandom (takeUptoOne c1)
  m' <- getStdRandom (takeUptoOne c2)
  m'' <- getStdRandom (takeUptoOne c3)
  m''' <- getStdRandom (takeUptoOne c4)
  ms <- getStdRandom (takeHV s')
  putStrLn $ showTikz $ sumUpSame $ (m ++ m' ++ m'' ++ m''' ++ ms)
  return ()

-- https://wiki.haskell.org/Random_shuffle

fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((M.insert j x . M.insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ L.foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate s = (zip [1..] s)
    initial x gen = (M.singleton 0 x, gen)
