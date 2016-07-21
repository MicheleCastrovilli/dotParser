module Main where

import Data.GraphViz 

main :: IO ()
main = do 
  a <- getContents 
  print $ parse a

