{-# LANGUAGE ScopedTypeVariables #-}
-- cabal install split
module Main where

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.List
import Data.List.Split
import Data.Maybe

import Data.Text (pack)
import qualified Data.Text.IO as T

import Control.Applicative
import Control.Monad
import Debug.Trace

type Kanji = String
type Recipe = String

toposort :: Eq a => Map a [a] -> [[a]]
toposort m = reverse $ snd $ until (null . fst) freeNodes (m, [])
    where freeNodes :: Eq a => (Map a [a], [[a]]) -> (Map a [a], [[a]])
          freeNodes (m, result) = (dependent, free:result)
              where (free_m, dependent_m) = M.partition null m
                    free = M.keys free_m
                    dependent = M.map (\\ free) dependent_m

testToposort :: [String] -- list of test failures
testToposort = mapMaybe check tests
    where check (x,y) = if toposort (M.fromList x) == y then Nothing else Just $ show x
          free i = (i, []) -- for legibility
          tests = [([], []),
                   ([(0, [1,2]), free 1, free 2], [[1,2],[0]]),
                   (
                       [free 1, free 2, free 3, (4, [2]), (5, [1,3]), (6, [1,4])],
                        [[1,2,3], [4,5], [6]]
                   )]

parseChise :: [String] -> Map Kanji Recipe
parseChise chiseLines =
    let
        isComment line = null line || take 1 line == ";"
        recipeLines = filter (not . isComment) chiseLines
        pairs = [(k, r) | line <- recipeLines, let (_:k:r:_) = splitOn "\t" line]
    in
        M.fromList pairs

main :: IO ()
main = do
    when (not $ null testToposort) $ error $ "test(s) failed: " ++ show testToposort
    chise <- readFile "chise.txt"
    (frequencyList :: [Kanji]) <- lines <$> readFile "frequent-joyo.txt"
    print (length frequencyList)
    let recipes = parseChise (lines chise)
    print (M.size recipes)
    mapM_ (T.putStrLn . pack) recipes
