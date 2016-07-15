module Main where

import CFG
import Diagrams.Backend.Cairo.CmdLine

main :: IO ()
main = do mainWith cfgDiagram
