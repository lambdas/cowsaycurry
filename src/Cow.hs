module Cow (constructCow) where

import           Data.List       (intercalate)
import           Data.List.Split (splitOn)

constructCow :: String -> IO String
constructCow name = do
    let path = "cows/" ++ name ++ ".cow"
    rawCow <- readFile path
    return $ processCow rawCow

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace searchFor replaceBy = intercalate replaceBy . splitOn searchFor

processCow :: String -> String
processCow rawCow = replace "$tongue" "  " . replace "$eyes" "oo" . replace "$thoughts" "\\" . replace "\\\\" "\\" . unlines . tail . init . lines $ rawCow
