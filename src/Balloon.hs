module Balloon (constructBalloon) where

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
