module Main where

import           Balloon (constructBalloon)
import           Cow     (constructCow)

main :: IO ()
main = do
    cow <- constructCow "default"
    putStrLn $ constructBalloon "hihihihihi" 3
    putStrLn cow
