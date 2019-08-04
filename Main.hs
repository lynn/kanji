{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
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

import Control.Arrow
import Control.Applicative
import Control.Monad
import Debug.Trace
import qualified Text.Parsec as P

type Parser = P.Parsec String ()

type Kanji = String
type RecipeString = String

data Recipe a = Atom a | IDC Char (Recipe a) (Recipe a)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Recipe' = Recipe String

----------
-- topo --
----------

toposort :: Eq a => Map a [a] -> [[a]]
toposort m = reverse $ snd $ until (null . fst) freeNodes (m, [])
    where freeNodes :: Eq a => (Map a [a], [[a]]) -> (Map a [a], [[a]])
          freeNodes (m, result) = if dependent == m then (M.empty, []) -- cycle check
                                  else (dependent, free:result)
              where (free_m, dependent_m) = M.partition null m
                    free = M.keys free_m
                    dependent = M.map (\\ free) dependent_m

testToposort :: [TestFailure]
testToposort = mapMaybe (test toposort) $ map (first M.fromList) tests
    where free i = (i, []) -- for legibility
          tests = [([], []),
                   ([(0, [1,2]), free 1, free 2], [[1,2],[0]]),
                   ([free 0, (1, [0,0])], []), -- duplicate in list (deemed illegal)
                   ([(1, [2]), (2, [1])], []), -- cycle
                   (
                       [free 1, free 2, free 3, (4, [2]), (5, [1,3]), (6, [1,4])],
                        [[1,2,3], [4,5], [6]]
                   )]

-----------
-- parse --
-----------

parseChise :: [String] -> Map Kanji RecipeString
parseChise chiseLines =
    let
        isComment line = null line || take 1 line == ";"
        recipeLines = filter (not . isComment) chiseLines
        pairs = [(k, r) | line <- recipeLines, let (_:k:r:_) = splitOn "\t" line]
    in
        M.fromList pairs

isIDC :: Char -> Bool
isIDC c = c `elem` ['⿰'..'⿻']

parseIdentifier :: Parser Recipe'
parseIdentifier = do
    P.char '&'
    name <- P.many (P.noneOf ";")
    P.char ';'
    pure (Atom $ "&" ++ name ++ ";")

parseKanji :: Parser Recipe'
parseKanji = Atom . pure <$> P.satisfy (not . isIDC)

parseAtom :: Parser Recipe'
parseAtom = parseIdentifier <|> parseKanji

parseIDC :: Parser Recipe'
parseIDC = do
    idc <- P.satisfy isIDC
    left <- parseRecipe
    right <- parseRecipe
    pure (IDC idc left right)

parseRecipe :: Parser Recipe'
parseRecipe = parseIDC <|> parseAtom

testParser :: [TestFailure]
testParser = mapMaybe (test $ P.parse parseRecipe "") $ map (second Right) tests
    where tests = [("x", Atom "x"),
                   ("⿰氵&CDP-8BD3;", IDC '⿰' (Atom "氵") (Atom "&CDP-8BD3;")), -- 漢
                   ("⿱宀子", IDC '⿱' (Atom "宀") (Atom "子")), -- 字
                   -- TODO: handle trinary idcs
                   -- ("⿳⿲木缶木冖⿰鬯彡", -- 鬱
                   --  IDC '⿳'
                   --      [IDC '⿲' $ map Atom ["木","缶","木"],
                   --       Atom "冖",
                   --       IDC '⿰' $ map Atom ["鬯","彡"]])
                   -- )
                   ("⿻⿻xy⿻zw",
                    IDC '⿻' (IDC '⿻' (Atom "x") (Atom "y"))
                             (IDC '⿻' (Atom "z") (Atom "w")))
                  ]

------------------
-- test harness --
------------------

data TestFailure = forall a. (Show a) => TestFailure a
instance Show TestFailure where show (TestFailure x) = show x

test :: (Show a, Eq b) => (a -> b) -> (a, b) -> Maybe TestFailure
test f (input, expected) = guard (f input /= expected) >> Just (TestFailure input)

testResults = testToposort ++ testParser

main :: IO ()
main = do
    when (not $ null testResults) $ error $ "test(s) failed: " ++ show testResults
    chise <- readFile "chise.txt"
    (frequencyList :: [Kanji]) <- lines <$> readFile "frequent-joyo.txt"
    print (length frequencyList)
    let recipes = parseChise (lines chise)
    print (M.size recipes)
