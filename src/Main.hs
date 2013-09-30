module Main where

import           System.Environment (getArgs)

import           Balloon            (constructBalloon)
import           Cow                (constructCow)

main :: IO ()
main = do
    args <- getArgs
    cow <- constructCow "default"
    putStrLn $ constructBalloon (unwords args) 20
    putStrLn cow
