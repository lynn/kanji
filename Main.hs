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
    chise <- readFile "chise.txt"
    (frequencyList :: [Kanji]) <- lines <$> readFile "frequent-joyo.txt"
    print (length frequencyList)
    let recipes = parseChise (lines chise)
    print (M.size recipes)
    mapM_ (T.putStrLn . pack) recipes
