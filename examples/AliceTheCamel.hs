{-# OPTIONS_GHC -fforce-recomp -F -pgmF "tophat" #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Tophat
import Tophat.Text

aliceTheCamel :: Template Int Text
aliceTheCamel = {{alicethecamel.tpt.txt}}

main :: IO ()
main = T.putStr $ runTemplate aliceTheCamel 3
