module Balloon (constructBalloon) where

import           Control.Applicative ((<$>))
import           Control.Monad       (join)
import           Data.Function       (on)
import           Data.List           (maximumBy)
import           Data.List.Split     (chunksOf)

constructBalloon :: String -> Int -> String
constructBalloon rawMessage maxWidth =
        unlines . addBorder . stringToBlock innerWidth $ rawMessage
    where innerWidth = min widestStringLength maxWidth
          widestStringLength = length . longestLine $ rawMessage

addBorder :: [String] -> [String]
addBorder = addTopBottomBorder . addSideBorder

addTopBottomBorder :: [String] -> [String]
addTopBottomBorder []          = []
addTopBottomBorder input@(x:_) = top : input ++ [bottom]
    where width  = length x - 4 -- minus side border width
          top    = " _" ++ replicate width '_' ++ "_"
          bottom = " -" ++ replicate width '-' ++ "-"

addSideBorder :: [String] -> [String]
addSideBorder []     = []
addSideBorder [x]    = ["< " ++ x ++ " >"]
addSideBorder (x:xs) = firstLine : middleLines ++ [lastLine]
    where firstLine     = enclose "/ " " \\" x
          lastLine      = enclose "\\ " " /" (last xs)
          middleLines   = enclose "| " " |" <$> (init . tail $ xs)
          enclose l r s = l ++ s ++ r

stringToBlock :: Int -> String -> [String]
stringToBlock width input = join . map (wrapAndPad width ' ') . lines $ input

longestLine :: String -> String
longestLine = maximumBy (compare `on` length) . lines

wrapAndPad :: Int -> a -> [a] -> [[a]]
wrapAndPad width filler input = pad width filler <$> wrap width input

wrap :: Int -> [a] -> [[a]]
wrap = chunksOf

pad :: Int -> a -> [a] -> [a]
pad width filler input
        | delta > 0 = input ++ replicate delta filler
        | otherwise = input
    where delta = width - length input

