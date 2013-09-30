module Main where

import Control.Applicative ((<$>))
import Data.List (intercalate)
import Data.List.Split (splitOn)

main::IO()
main = do
    putStrLn $ constructBalloon "hihihihihi" 3
    rawCow <- readFile "cows/default.cow"
    putStrLn $ processCow rawCow

constructBalloon :: String -> Int -> String
constructBalloon rawMessage maxWidth = 
        topBorder ++ "\n" ++ message {-++ "\n" -}++ bottomBorder
    where topBorder = " _" ++ replicate innerWidth '_' ++ "_"
          bottomBorder = " -" ++ replicate innerWidth '-' ++ "-"
          message = unlines . addBorders . map pad . wrapLines . lines $ rawMessage
          addBorders (first:rest)
              | null rest = [addBorder '<' '>' first]
              | otherwise = addBorder '/' '\\' first : map (addBorder '|' '|') (tail . init $ rest) ++ [addBorder '\\' '/' (last rest)]
          addBorders x = x
          addBorder l r s = l:" " ++ s ++ ' ':r:""
          wrapLines ss = ss >>= wrap
          wrap s 
              | length s > maxWidth = first : wrap rest
              | otherwise = [s]
              where (first, rest) = splitAt maxWidth s
          pad s
              | delta > 0 = s ++ replicate delta ' '
              | otherwise = s
              where delta = innerWidth - length s
          innerWidth = min widestStringLength maxWidth
          widestStringLength = maximum . map length . lines $ rawMessage

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace searchFor replaceBy input = intercalate replaceBy $ splitOn searchFor input

replaceFirst :: Eq a => [a] -> [a] -> [a] -> [a]
replaceFirst searchFor replaceBy input = before ++ replaceBy ++ after
    where (before:rest) = splitOn searchFor input
          after = intercalate searchFor rest

processCow :: String -> String
processCow rawCow = replace "$tongue" "  " . replace "$eyes" "oo" . replace "$thoughts" "\\" . replace "\\\\" "\\" . unlines . tail . init . lines $ rawCow

