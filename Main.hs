{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
module Main where

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import Data.List
import Data.Maybe
import Data.Either
import Data.Foldable
import System.Directory
import System.FilePath

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

data Recipe a = Atom a
              | IDC Char (Recipe a) (Recipe a)
              | IDC3 Char (Recipe a) (Recipe a) (Recipe a)
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Recipe' = Recipe String

----------
-- 整理 --
----------

toposort :: Eq a => Map a [a] -> [[a]]
toposort m = reverse $ snd $ until (null . fst) freeNodes (m, [])
    where freeNodes :: Eq a => (Map a [a], [[a]]) -> (Map a [a], [[a]])
          freeNodes (m, result) = if dependent == m then (M.empty, []) -- サイクル・チェック
                                  else (dependent, free:result)
              where (free_m, dependent_m) = M.partition null m
                    free = M.keys free_m
                    dependent = M.map (\\ free) dependent_m

testToposort :: [TestFailure]
testToposort = mapMaybe (test toposort) $ map (first M.fromList) tests
    where free i = (i, []) -- 読みやすいように
          tests = [([], []),
                   ([(0, [1,2]), free 1, free 2], [[1,2],[0]]),
                   ([free 0, (1, [0,0])], []), -- ダブる（違反だという）
                   ([(1, [2]), (2, [1])], []), -- サイクル
                   (
                       [free 1, free 2, free 3, (4, [2]), (5, [1,3]), (6, [1,4])],
                        [[1,2,3], [4,5], [6]]
                   )]

----------
-- 解析 --
----------

isIDC :: Char -> Bool
isIDC c = c `elem` ['⿰'..'⿻'] \\ "⿲⿳"

isIDC3 :: Char -> Bool
isIDC3 c = c `elem` "⿲⿳"

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

parseIDC3 :: Parser Recipe'
parseIDC3 = liftM4 IDC3 (P.satisfy isIDC3) parseRecipe parseRecipe parseRecipe

parseIDC :: Parser Recipe'
parseIDC = liftM3 IDC (P.satisfy isIDC) parseRecipe parseRecipe

parseRecipe :: Parser Recipe'
parseRecipe = parseIDC3 <|> parseIDC <|> parseAtom

parseChiseEntry :: Parser (Kanji, Recipe')
parseChiseEntry = do
    name    <- P.manyTill P.anyChar P.tab
    kanji   <- P.manyTill P.anyChar P.tab
    recipe  <- parseRecipe
    comment <- P.manyTill P.anyChar P.endOfLine -- たとえばIDS-UCS-Basic.txt:887の"\t?"
    pure (kanji, recipe)

parseChiseLine :: Parser (Kanji, Recipe')
parseChiseLine = P.skipMany commentLine >> parseChiseEntry
    where commentLine = P.char ';' >> P.manyTill P.anyChar P.endOfLine

parseChiseMap :: Parser (Map Kanji Recipe')
parseChiseMap = M.fromList <$> (P.many parseChiseLine <* P.eof)

-- 無事に解析した漢字のマップとともに、解析できなかったラインのリストを返す。
-- ちょっと適当なんだけど。
parseChiseMapWithErrors :: String -> (Map Kanji Recipe', [String])
parseChiseMapWithErrors input = (recipes, errors)
    where try line = case P.parse parseChiseMap "" (line ++ "\n") of
                         Left _ -> Left line
                         Right _ -> Right line
          notComment line = take 1 line /= ";"
          (errors, oks) = partitionEithers $ map try $ filter notComment $ lines input
          Right recipes = P.parse parseChiseMap "" $ unlines oks

testRecipeParser :: [TestFailure]
testRecipeParser = mapMaybe (test $ P.parse parseRecipe "") $ map (second Right) tests
    where idc c x y = IDC c (Atom x) (Atom y)
          idc3 c x y z = IDC3 c (Atom x) (Atom y) (Atom z)
          tests = [("x", Atom "x"),
                   ("⿰氵&CDP-8BD3;", idc '⿰' "氵" "&CDP-8BD3;"), -- 漢
                   ("⿱宀子", idc '⿱' "宀" "子"), -- 字
                   ("⿳⿲木缶木冖⿰鬯彡", -- 鬱
                    IDC3 '⿳'
                        (idc3 '⿲' "木" "缶" "木")
                        (Atom "冖")
                        (idc '⿰' "鬯" "彡")
                   ),
                   ("⿻⿻xy⿻zw",
                    IDC '⿻' (idc '⿻' "x" "y")
                             (idc '⿻' "z" "w"))
                  ]

testChiseParser :: [TestFailure]
testChiseParser = catMaybes [test (P.parse parseChiseMap "") (input, output)]
    where input  = "U+5160\t兠\t⿱⿲&CDP-8BC5;白匕儿\nU+5161\t兡\t⿺克百\n"
          output = Right $ M.fromList
            [ ("兠", IDC '⿱' (IDC3 '⿲' (Atom "&CDP-8BC5;") (Atom "白") (Atom "匕")) (Atom "儿"))
            , ("兡", IDC '⿺' (Atom "克") (Atom "百")) ]

----------------------
-- テスト・ハーネス --
----------------------

data TestFailure = forall a. (Show a) => TestFailure a
instance Show TestFailure where show (TestFailure x) = show x

test :: (Show a, Eq b) => (a -> b) -> (a, b) -> Maybe TestFailure
test f (input, expected) = guard (f input /= expected) >> Just (TestFailure input)

testResults = testToposort ++ testRecipeParser ++ testChiseParser

--------------
-- レシピー --
--------------

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f a =
#ifdef HIGHLY_OPTIMIZED
    f (f (f a))
#else
    let a' = f a in if a == a' then a else fixedPoint f a'
#endif

readRecipes :: (Kanji -> Bool) -> IO (Map Kanji Recipe')
readRecipes isJoyo = do
    chiseFiles <- getDirectoryContents "chise-ids"
    let isIncluded name = any (`isPrefixOf` name) ["IDS-UCS", "IDS-CDP", "IDS-CNS"]
    let sources = map ("chise-ids" </>) $ filter isIncluded chiseFiles
    chiseText <- concat <$> mapM readFile sources
    let (recipes, all_errors) = parseChiseMapWithErrors chiseText
    -- 適当に再びラインずつ漢字をパースする
    -- TODO: もし常用じゃなくても他の常用漢字に含まれた部分のエラーを除いちゃわないように
    let errors = filter (isJoyo . take 1 . drop 1 . dropWhile (/= '\t')) all_errors
    when (not $ null errors) $ do
        putStrLn "Some joyo lines failed to parse:"
        mapM_ (T.putStrLn . pack) errors
        putStrLn ""
    return recipes

readFrequentJoyo :: IO [Kanji]
readFrequentJoyo = lines <$> readFile "frequent-joyo.txt"

-- レシピーのマップと漢字のリスとを与えられて、
-- その漢字の部分を引きつづけると、やがてその漢字が分解されるアトムのセットを返す。
atomSet :: Map Kanji Recipe' -> [Kanji] -> [Kanji]
atomSet recipes ks = sort $ fixedPoint iterate ks
    where iterate = nub . concatMap (toList . lookup)
          lookup k = M.findWithDefault (Atom k) k recipes  -- レシピーが見つからないと、不可分だとする。

main :: IO ()
main = do
    when (not $ null testResults) $ error $ "test(s) failed: " ++ show testResults
    frequentJoyo <- readFrequentJoyo
    recipes <- readRecipes (`elem` frequentJoyo)

    -- アトムのリストを手に入れるまで、`frequentJoyo`というリストのすべてを分解する。
    let atoms = atomSet recipes frequentJoyo
    let isForeign k = M.notMember k recipes
    putStrLn $ "Atoms: " ++ unwords atoms
    putStrLn ""
    putStrLn $ "Foreign atoms: " ++ unwords (filter isForeign atoms)
