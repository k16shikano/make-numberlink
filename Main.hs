module Main where

import System.Environment (getArgs)
import System.Random
import Data.Map as M
import Data.List as L

-- take arbitrary number of pairs from
s = [(0,1), (1,2), (2,3),
     (4,5), (5,6), (6,7),
     (8,9), (9,10), (10,11),
     (12,13), (13,14), (14,15),
     (0,4), (1,5), (2,6), (3,7),
     (4,8), (5,9), (6,10), (7,11),
     (8,12), (9,13), (10,14), (11,15)
     ]

-- take zero or one pair from each
c1 = [(0,5), (1,4)]
c2 = [(1,6), (2,5)]
c3 = [(2,7), (3,6)]
c4 = [(4,9), (5,8)]
c5 = [(5,10), (6,9)]
c6 = [(6,11), (7,10)]
c7 = [(8,13), (9,12)]
c8 = [(9,14), (10,13)]
c9 = [(10,15), (11,14)]

takeWith :: RandomGen g => [(Int, Int)] -> [Bool] -> g -> ([(Int, Int)], g)
takeWith l preds g = (L.map snd $ L.filter fst (L.zip shuffled l), g')
  where (shuffled, g') = fisherYates g preds

takeHV n g = takeWith s (replicate (24 - n) False ++ L.replicate n True) g

takeUptoOne :: RandomGen g => [(Int, Int)] -> g -> ([(Int, Int)], g)
takeUptoOne l g = case randomR (1 :: Int, 3 :: Int) g of
                    (1,g') -> takeWith l [True, False] g'
                    (2,g') -> takeWith l [False, True] g'
                    (3,g') -> takeWith l [False, False] g'

flatten :: [(Int, Int)] -> [Int]
flatten l = concatMap (\p -> [fst p, snd p]) l

complement :: [[Int]] -> [Int]
complement s = L.map (\n -> count n) [0..15]
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
  "\\draw (4,0) node {" ++ (show $ l !! 1) ++ "};" ++
  "\\draw (8,0) node {" ++ (show $ l !! 2) ++ "};" ++
  "\\draw (12,0) node {" ++ (show $ l !! 3) ++ "};" ++
  "\\draw (0,4) node {" ++ (show $ l !! 4) ++ "};" ++
  "\\draw (4,4) node {" ++ (show $ l !! 5) ++ "};" ++
  "\\draw (8,4) node {" ++ (show $ l !! 6) ++ "};" ++
  "\\draw (12,4) node {" ++ (show $ l !! 7) ++ "};" ++
  "\\draw (0,8) node {" ++ (show $ l !! 8) ++ "};" ++
  "\\draw (4,8) node {" ++ (show $ l !! 9) ++ "};" ++
  "\\draw (8,8) node {" ++ (show $ l !! 10) ++ "};" ++
  "\\draw (12,8) node {" ++ (show $ l !! 11) ++ "};" ++
  "\\draw (0,12) node {" ++ (show $ l !! 12) ++ "};" ++
  "\\draw (4,12) node {" ++ (show $ l !! 13) ++ "};" ++
  "\\draw (8,12) node {" ++ (show $ l !! 14) ++ "};" ++
  "\\draw (12,12) node {" ++ (show $ l !! 15) ++ "};" ++
  "\\end{tikzpicture}" ++
  "\\end{document}"

main :: IO ()
main = do
  s:s':_ <- fmap (L.map read) getArgs
  setStdGen (mkStdGen s)
  m1 <- getStdRandom (takeUptoOne c1)
  m2 <- getStdRandom (takeUptoOne c2)
  m3 <- getStdRandom (takeUptoOne c3)
  m4 <- getStdRandom (takeUptoOne c4)
  m5 <- getStdRandom (takeUptoOne c5)
  m6 <- getStdRandom (takeUptoOne c6)
  m7 <- getStdRandom (takeUptoOne c7)
  m8 <- getStdRandom (takeUptoOne c8)
  m9 <- getStdRandom (takeUptoOne c9)
  ms <- getStdRandom (takeHV s')
  putStrLn $ showTikz $ sumUpSame $ (m1 ++ m2 ++ m3 ++ m4 ++ m5 ++ m6 ++ m7 ++ m8 ++ m9 ++ ms)
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
