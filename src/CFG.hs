module CFG (drawPaths, drawPath, drawBlock, cfgDiagram) where

import Diagrams.Prelude hiding (over)
import Diagrams.Backend.Cairo.CmdLine
import Data.Colour

darkTeal :: Colour Double
darkTeal = sRGB24read "#23373b"

textColor :: Colour Double
textColor = over (withOpacity black 0.02) white

paths :: [[Int]]
paths = [[0,1],[1,2,4,6,1],[1,2,5,6,1],[1,2,5,6,1],[1,3]]

text' :: String -> Diagram B
text' t = text t # fc textColor # font "Fira Sans"

drawBlock :: String -> Int -> Diagram B
drawBlock name n =
  ((text' (show n)) <> (square 1 # fc darkTeal # lw 0)) # named name

drawPath :: String -> [Int] -> Diagram B
drawPath prefix nodes =
  hsep 1
       (zipWith (\n' i -> drawBlock (prefix <> show i) n')
                nodes
                [1::Int ..]) #
  applyAll [connectOutside'
              (with & headStyle %~ fc darkTeal & shaftStyle %~ lc darkTeal)
              (prefix <> show i)
              (prefix <> show j)
           |(i,j) <-
              zip [1 .. n]
                  [2::Int ..]]
  where n = length nodes

drawPaths :: [[Int]] -> Diagram B
drawPaths ps =
  vsep 1 $
  zipWith (\p i -> drawPath (show i <> "_") p)
          ps
          [1 :: Int ..]

cfgDiagram :: Diagram B
cfgDiagram = drawPaths paths
