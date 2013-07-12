{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

circle1 = circle 1
  # fc blue
  # lw 0.05
  # lc purple
  # dashing [0.2,0.05] 0

pCircle1 = circle1 # pad 1.1
circleSq = square 1 # fc aqua `atop` pCircle1

main = defaultMain circleSq
