{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String (fromString)
import Data.Text hiding (replicate, reverse)
import Template


{-
  This is just supposed to be an example of what templates should
  compile to.
-}



example :: Template Int Text
example = makeTemplate (
  forH (\ n -> reverse [0..n]) >>>
    forH (replicate 3) >>>
      embedConst "Alice the camel has " >>>
      embed (pack . show) >>>
      embedConst " humps\n" >>>
    endfor >>>
    ifH (> 0) >>>
      embedConst "So go, Alice, go!\n\n" >>>
    endif >>>
    ifH (== 0) >>>
      embedConst "Because Alice is a horse\n\n" >>>
    endif >>>
  endfor)


main :: IO ()
main = putStrLn . unpack $ runTemplate example 3
