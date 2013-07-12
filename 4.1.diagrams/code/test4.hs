{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

circleRect =
  circle 1 # scale 0.5
  |||
  square 1 # scaleX 0.3 
           # rotateBy (1/6) 
           # scaleX 0.5

main = defaultMain circleRect
