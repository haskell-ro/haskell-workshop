% Writing fast Haskell programs
% Lucian Mogosanu
% 11.07.2014

# The Programming Trilemma

* Fast
* Correct
* Easy to set up

## Pick two of the above.

# Performance

Why do programmers ignore it?

* Knowledge about the language(s) (or lack thereof)
* Knowledge about the underlying hardware (or lack thereof)
* Deadlines (or lack thereof)

## "We will encourage you to develop the three great virtues of a programmer:
laziness, impatience, and hubris." -- Larry Wall

# Trade-offs

* "Low-level" languages are difficult to use
* "High-level" languages tend to abstract too much

. . .

## Example: Memory management

* Pointers
* Garbage collection

# Performance of Haskell programs (1)

* Haskell code can be compiled down to native binaries
* (Simplified) compilation steps:

> 1. Parsing and type checking $\rightarrow$ Fully annotated Haskell code
> 2. Desugaring and simplification $\rightarrow$ Core code
> 3. Backend code generation $\rightarrow$ Cmm code
> 4. Backend optimization and compilation $\rightarrow$ Binary

# Peformance of Haskell programs (2)

* It's not (very) difficult to generate efficient code from Haskell
* ... but there are quirks

## "Premature optimization is the root of all evil" -- Donald Knuth

# Performance of Haskell programs (3)

Main problems:

* (Stack) recursion
* Lazy evaluation (not always, though)
* Data encapsulation (remember `newtype`)

# Example: Computing the nth Fibonacci term (1)

* $F_0 = 0, F_1 = 1$
* $F_n = F_{n-1} + F_{n-2}, n > 1$

~~~~ {.haskell}
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
~~~~

# Example: Computing the nth Fibonacci term (2)

~~~~
> fib 4
> fib 10
> fib 30
~~~~

What are the evaluation steps for `fib 4`?

# Example: Computing the nth Fibonacci term (3)

> * `fib 4 = fib 3 + fib 2`
> * `      = (fib 2 + fib 1) + (fib 1 + fib 0)`
> * `      = ((fib 1 + fib 0) + fib 1) + (fib 1 + fib 0)`
> * `      = ((1 + 0) + 1) + (1 + 0)`
> *	`      = 3`

# Example: Computing the nth Fibonacci term (4)

Problems:

* Stack recursion
* Possible space leak due to lazy evaluation
* (Obviously) speed due to the above

# Example: Computing the nth Fibonacci term (5)

A better™ fib (using tail recursion):

~~~~ {.haskell}
betterFib :: Num a => Int -> a
betterFib n
  | n == 0    = 0
  | otherwise = go (0,1) 1
  where
  go (f0, f1) n1 = if n1 == n
    then f1
    else go (f1, f0 + f1) (n1 + 1)
~~~~

* What are the evaluation steps for `betterFib 4`?

# Example: Computing the nth Fibonacci term (6)

> * `betterFib 4 = go (0,1) 1`
> * `            = go (1,1) 2`
> * `            = go (1,2) 3`
> * `            = go (2,3) 4`
> * `            = 3`

. . .

* Pro: solves the above problems
* Cons: slightly uglier
* Solution: idiomatic code (`fold`, `map`, `zip` etc.)

# Example: Summing a million numbers (1)

~~~~ {.haskell}
mySum []       = 0
mySum (x : xs) = x + mySum xs
~~~~

. . .

Problem: looks like a right fold

~~~~
mySum [1..n] = 1 + (2 + (3 + (4 + ..)))
~~~~

# Example: Summing a million numbers (2)

Tail recursive version:

~~~~ {.haskell}
mySum2 = go 0
  where
  go acc []       = acc
  go acc (x : xs) = go (x + acc) xs
~~~~

. . .

Problem: `acc`'s evaluation is delayed until the very end

* We have a memory leak!

~~~~
mySum2 [1..n] = go (1 + (2 + (3 + (4 + ..)))) [..]
~~~~

# Example: Summing a million numbers (3)

* Force the evaluation of `x + acc`

~~~~ {.haskell}
seq :: a -> b -> b
~~~~

* Forces the evaluation of the 1st argument and returns the 2nd

# Example: Summing a million numbers (4)

Strict, tail recursive version:

~~~~ {.haskell}
mySum3 = go 0
  where
  go acc []       = acc
  go acc (x : xs) = seq (x + acc) $ go (x + acc) xs
~~~~

**Note**: for lists in `Enum`, GHC will optimize away lists

# Example: Summing a million numbers (5)

* What about `foldl`?
* Same problem as `mySum2`

. . .

* Solution: `foldl'` as a strict version of `foldl`
* **Don't** use `foldr`!

# Example: Folding left and right (1)

`foldl` versus `foldr` $\leftrightarrow$ tail versus stack recursion

~~~~
foldl: (.. ((a `f` b) `f` c) `f` d) ..
foldr: a `f` (b `f` (c `f` (d `f` ..))
~~~~

Different results for non-associative operations!

# Example: Folding left and right (2)

* `foldl` has no problem adding 10,000 numbers
* What about other operators?
* Example: concatenate 10,000 lists

~~~~
> foldl (++) [] $ replicate 10000 [1,2,3]
> foldr (++) [] $ replicate 10000 [1,2,3]
~~~~

# Example: Folding left and right (3)

Going deeper:

~~~~ {.haskell}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc []       = acc
foldr f acc (x : xs) = x `f` foldr f acc xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc []       = acc
foldl f acc (x : xs) = foldl f (acc `f` x) xs
~~~~

# Example: Folding left and right (4)

Left fold:

~~~~
.. ((([] ++ [1,2,3]) ++ [1,2,3]) ++ [1,2,3]) ..
~~~~

How is `(++)` implemented?

# Example: Folding left and right (5)

~~~~ {.haskell}
(++) :: [a] -> [a] -> [a]
(++) [] l2       = l2
(++) (x : xs) l2 = x : (++) xs l2
~~~~

* Recursion is done on 1st argument
* Elements are added to the left

# Example: Folding left and right (6)

Left fold:

~~~~
.. ((([] ++ [1,2,3]) ++ [1,2,3]) ++ [1,2,3]) ..
~~~~

Right fold:

~~~~
[1,2,3] ++ ([1,2,3] ++ ([1,2,3] ++ (.. ([1,2,3] ++ []) ..)))
~~~~

# Example: Folding left and right (7)

Left fold:

~~~~
([1,2,3,1,2,3,1,2,3,..] ++ [1,2,3]) ++ ..
~~~~

Right fold:

~~~~
.. ++ ([1,2,3] ++ [1,2,3,1,2,3,1,2,3,..]
~~~~

# Example: Folding left and right (8)

For $n$ constant-length lists:

* `foldl` $\rightarrow$ $O(n^2)$ time
* `foldr` $\rightarrow$ $O(n)$ time

# Extra: Profiling with GHC

* `apt-get install ghc-prof`

~~~~
$ ghc -prof Fib.hs
$ ./Fib +RTS -p
$ cat Fib.prof
~~~~

# Extra: ghc-core (1)

* Holistic optimization: looking at the assembly code
* However, assembly code generated by Haskell is unreadable
* Possible solution: look at intermediate code

# Extra: ghc-core (2)

* `cabal install ghc-core`

~~~~
$ ghc-core Fib.hs
~~~~

# Extra: ghc-core (3)

* `fib`:

~~~~ {.haskell}
$wfib =
  \ (ww_s1PB :: Int#) ->
    case ww_s1PB of wild_XP {
      __DEFAULT ->
        Type.plusInteger
          ($wfib (-# wild_XP 1))
          ($wfib (-# wild_XP 2));
      0 -> main7;
      1 -> main6
    }
~~~~

# Extra: ghc-core (4)

* `betterFib`/`go` (with `n == 40`)

~~~~ {.haskell}
$wgo =
  \ (ww_s1Pq :: Type.Integer)
    (ww1_s1Pr :: Type.Integer)
    (ww2_s1Pv :: Int#) ->
    case ww2_s1Pv of wild_XO {
      __DEFAULT ->
        $wgo
          ww1_s1Pr
          (Type.plusInteger ww_s1Pq ww1_s1Pr)
          (+# wild_XO 1);
      40 -> ww1_s1Pr
    }
~~~~
