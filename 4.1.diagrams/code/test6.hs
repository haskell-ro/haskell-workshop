{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

hilbert = iterate expand mempty where
  expand t = alignBL $ hcat [u, hrule 1, reflectX u] where
             u = vcat [t, alignT $ vrule 1, rotateBy (3/4) t]

example = pad 1.1 . centerXY . lw 0.05 $ hilbert!!5

main = defaultMain example
