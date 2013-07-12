{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Diagrams.Prelude
import Diagrams.TwoD.Layout.Tree
import Diagrams.Backend.SVG.CmdLine

fibCalls :: Integer -> BTree Integer
fibCalls 0 = leaf 0
fibCalls 1 = leaf 1
fibCalls n = BNode n (fibCalls (n-1)) (fibCalls (n-2))

Just t = uniqueXLayout 2 2 (fibCalls 5)

example = pad 1.1 . lw 0.03 . centerXY 
        $ renderTree 
            (\n -> (text ("fib " ++ show n)
                    <> roundedRect 3 1.3 0.3 # fc white)
            )
            (~~) t

main = defaultMain example
