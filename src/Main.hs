module Main where

import Control.Monad (guard)
import Data.Maybe (listToMaybe)
import System.Environment (getArgs)
import System.FilePath (FilePath)
import qualified System.FilePath as FilePath
import Text.Read (readMaybe)

-- | Old indentation size (number of spaces per indent)
newtype OldIndSize = OldIndSize Int

-- | New indentation size (number of spaces per indent)
newtype NewIndSize = NewIndSize Int

main :: IO ()
main = do
    args <- getArgs
    case (parseArgs args) of
        Nothing -> do
            putStrLn "usage: indenter file newfile oldindent newindent"
            putStrLn "example: indenter Main.hs Main2.hs 2 4"
            putStrLn "    This changes 2-space indented Main.hs into 4-space indented Main2.hs"
        Just (inputFilePath, outputFilePath, oldSize, newSize) -> do
            contents <- readFile inputFilePath
            case changeIndentation contents oldSize newSize of
                Left lineNums -> 
                    putStrLn $ "Invalid spacing on lines " ++ show lineNums
                Right newContents -> 
                    writeFile outputFilePath newContents

parseArgs :: [String] -> Maybe (FilePath, FilePath, OldIndSize, NewIndSize)
parseArgs [path1, path2, str1, str2] = do
    guard $ FilePath.isValid path1
    guard $ FilePath.isValid path2
    num1 <- readMaybe str1
    num2 <- readMaybe str2
    return (path1, path2, OldIndSize num1, NewIndSize num2)
parseArgs _ = Nothing

type InvalidLineNumbers = [Int]

countPrefixSpaces :: String -> Int
countPrefixSpaces s = length $ takeWhile (== ' ') s

addPrefixSpaces :: Int -> String -> String
addPrefixSpaces n s = prefix ++ s
  where
    prefix = replicate n ' '

hasValidIndentation :: OldIndSize -> String -> Bool
hasValidIndentation (OldIndSize n) s = (countPrefixSpaces s) `mod` n == 0

hasInvalidIndentation :: OldIndSize -> String -> Bool
hasInvalidIndentation oldSize s = not $ hasValidIndentation oldSize s

invalidLineNums :: String -> OldIndSize -> InvalidLineNumbers
invalidLineNums s oldSize = fmap fst $ filter snd lineNumsAndStatuses
  where
    statuses :: [Bool]
    statuses = fmap (hasInvalidIndentation oldSize) (lines s)
    lineNumsAndStatuses :: [(Int, Bool)]
    lineNumsAndStatuses = zip [1..] statuses

changeIndentation :: String
                  -> OldIndSize
                  -> NewIndSize
                  -> Either InvalidLineNumbers String
changeIndentation s oldSize newSize =
    case invalidLineNums s oldSize of
        [] -> Right . unlines $
            fmap (changeLineIndentation oldSize newSize) (lines s)
        invalidLineNums -> Left invalidLineNums

changeLineIndentation :: OldIndSize -> NewIndSize -> String -> String
changeLineIndentation (OldIndSize oldSize) (NewIndSize newSize) s =
    addPrefixSpaces totalSpaces (trimStart s)
  where
    indentLevel = (countPrefixSpaces s) `div` oldSize
    totalSpaces = indentLevel * newSize
    trimStart = dropWhile (== ' ')