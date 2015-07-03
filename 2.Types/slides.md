% Zen and the Art of Haskell Types
% Lucian Mogosanu
% 04.07.2015

# Questions

* What are types?

* Why do we need types/type checking?

* How do we make a friend of Haskell types?

# What are types?

Loose definition:

* Semantic annotations for data structures
	* "Bob is a person"
	* "$x$ is an integer"
	* "$g$ is a graph"

* ... but also for algorithms
	* "$f$ is a function"
	* "$p$ is a logical proposition"
	* "(another) $p$ is a program"

# Digression

* What types does your PC understand?

. . .

* Integers (sometimes with a sign)
* Memory addresses
* Instructions

# What types does your PC understand? (2)

* Consider the following sequence of instructions:

~~~~
1: r0 <- 0
2: r1 <- val@r0 ; get val from mem addr in r0
~~~~

. . .

* ... or the following

~~~~
1: r0 <- 0
2: goto instr@r0 ; jump to instr at mem addr in r0
~~~~

. . .

* What are the semantics of these programs?

# What types does your PC understand? (3)

* Consider the following sequence of instructions:

~~~~
1: r0 <- 0
2: r1 <- val@r0
~~~~

* `0` is a "special" **address**
	* but an ordinary **integer**!

. . .

* Accessing arbitrary addresses can cause programs to
	* fail
	* (*or worse*) misbehave
	* and (*even worse*) help the NSA spy on you.

# So, why do we need type checking?

* We're careless
* We need a tool to help us understand our code
	* "Programs must be written for people to read, and only incidentally for
	  machines to execute."
* We need to be able to reason about our programs
	* "Lightweight formal verification"

# Type Signatures

* In set theory, we would say, e.g. $-2 \in \mathbb{Z}$

* In Haskell, we say, `-2 :: Integer`

# Basic Haskell Types

~~~~ {.haskell}
3 :: Int
120894192912541294198294982 :: Integer
3.14 :: Float
'a' :: Char
"The cake is a lie" :: String
True :: Bool
~~~~

# Compound Types: Lists

~~~~ {.haskell}
[3, 4, 5] :: [Int]
['a', 'b'] :: [Char] -- String
[[3,4], []] :: [[Int]]
~~~~

# Compound Types: Lists (2)

* What happens if we write:

~~~~ {.haskell}
[3, 4, 'a', 'b'] :: [Int]
~~~~

. . .

* The type checker will scream:

~~~~ {.haskell}
Couldn't match expected type `Int' with actual type `Char'
In the expression: 'a'
In the expression: [3, 4, 'a', 'b'] :: [Int]
In an equation for `it': it = [3, 4, 'a', ....] :: [Int]
~~~~

* **expected** type: `Int`
* **actual** type: `Char`


# Compound Types: Pairs/Tuples

~~~~ {.haskell}
() :: ()
(3, "The cake is a lie") :: (Int, String)
(3, True, 3.14, "It's July") :: (Int, Bool, Float, [Char])
 (('a', 5), "A String") :: ((Char, Int), String)
~~~~

# Functions and Type Variables (1)

~~~~ {.haskell}
f :: Int -> Int
f x = ...
~~~~

* `(->)` is the function type
* `f` is a function receiving an `Int`, returning an `Int`

# Functions and Type Variables (2)

~~~~ {.haskell}
f :: Int -> Int
f x = x
~~~~

. . .

Why not:

~~~~ {.haskell}
f True
~~~~

# Functions and Type Variables (3)

~~~~ {.haskell}
f :: a -> a
f x = x
~~~~

. . .

* Capital letter $\rightarrow$ proper type
* Small letter $\rightarrow$ type variable (stands for multiple types)

We say that `f` is **polymorphic**

* Single implementation
* Multiple types

# Functions and Type Variables (4)

* Functions are curried

~~~~ {.haskell}
a -> b -> c
a -> (b -> c) -- same as above
(a -> b) -> c -- different type
~~~~

# Types as documentation (1)

* Which functions have type `a -> a`?
    * excluding dummy cases
        * `f x = error "undefined"`
        * `f x = undefined`
        * `f x = f x`
    * want the simplest possible expression
        * `f x = head [...]`

. . .

* The natural solution:

~~~~ {.haskell}
f x = x
~~~~


# Types as documentation (2)

* Which functions have type `(a, b) -> a`?

. . .

* A single solution:

~~~~ {.haskell}
f (x, y) = x
~~~~

# Types as documentation (3)

* What are the types of:

~~~~ {.haskell}
3
3.0
2 + 3
2 + 3.0
2 / 3
~~~~

. . .

* Signatures of `(+)`, `(/)`

# Consulting Types in GHCi

* :t expression

# Hayoo, Hoogle

* hoogle  
	[http://www.haskell.org/hoogle](http://www.haskell.org/hoogle)  
	[https://www.fpcomplete.com/hoogle](https://www.fpcomplete.com/hoogle)
* hayoo  
	[http://holumbus.fh-wedel.de/hayoo/hayoo.html](http://holumbus.fh-wedel.de/hayoo/hayoo.html)

# Programmer defined Types (1)

* How is a `String` a synonym of `[Char]`?

~~~~ {.haskell}
type String = [Char]
~~~~

. . .

~~~~ {.haskell}
type Point = (Double, Double)
type Size = (Double, Double)
type Vector = (Double, Double)
~~~~

~~~~ {.haskell}
area :: Size -> Double
area (x, y) = x * y

p :: Point
p = (3.14, 4.2)

area p
~~~~

# Programmer defined Types (2)

* Type synonyms help only at lexical level
* Compiler sees the base type and works with it

# Programmer defined Types (3)

* How do we build our own types?

~~~~ {.haskell}
data Colour = Red | Green | Blue | ...
~~~~

* "The type Colours can have the values Red, Green, Blue, and so on"

~~~~ {.haskell}
data Bool = True | False
~~~~

* A type is a collection of **constructors**
* Constructors start with capital letters

# Programmer defined Types (4)

* Constructors can have **fields**

~~~~ {.haskell}
data Person
    = Male String Int -- name and age
    | Female String String Int -- name, maiden name, age
~~~~

. . .

* Constructors are **functions**

~~~~ {.haskell}
Female :: String -> String -> Int -> Person
~~~~

~~~~ {.haskell}
Male "Winston Smith" :: Int -> Person
~~~~

# Programmer defined Types (5)

* Types can be defined in terms of type variables
	* **parametric types**
* Lists are parametric: `[a]`

~~~~ {.haskell}
data Container a = Empty | Holding a
~~~~

# Interesting Haskell Types

~~~~ {.haskell}
data Maybe a = Nothing | Just a
data Either a b = Left a | Right b
~~~~

# Recursive Data Types

* Naturals

~~~~ {.haskell}
data N = Zero | Succ N
~~~~

. . .

* Lists

~~~~ {.haskell}
data List a = EmptyL | ConsL a (List a)
~~~~

. . .

* Binary trees

~~~~ {.haskell}
data BinaryTree a =
      BTEmpty
    | BTNode (BinaryTree a) a (BinaryTree a)
~~~~

# Working with Programmer defined Types

~~~~ {.haskell}
data Container a = Empty | Holding a

isEmpty :: Container a -> Bool
isEmpty Empty           = True
isEmpty _               = False
-- isEmpty (Holding _ ) = False

place :: Container a -> a -> Container a
place Empty x   = Holding x
place _ _       = error "Container already full"
~~~~

# Space optimization

Scenario:

* My type has one constructor with one field, e.g.:

~~~~ {.haskell}
data OneFieldOnly a = Constructor a
type OneFieldOnly a = a
~~~~

. . .

* **`data`** -- needs too much extra-memory (constructor tags, thunk tags)
* **`type`** -- doesn't offer type safety
* **`newtype`** -- have the cookie and eat it too

~~~~ {.haskell}
newtype OneFieldOnly a = Constructor a
~~~~

# Smart Constructors

~~~~ {.haskell}
newtype Nat = MkNat { fromNat :: Int }
~~~~

* `MkNat` $\rightarrow$ can take negative values
* **Smart constructor** $\rightarrow$ function that performs extra checks on input

~~~~ {.haskell}
toNat :: Int -> Nat
~~~~

# Record Types (1)

~~~~ {.haskell}
data Person = P String String String
    Int Int Person Person
~~~~

* Which one is the father's name? (can use type synonyms)
* What happens if we:
	* Change field order by mistake?
	* Add another parameter?

# Record Types (2)

~~~~ {.haskell}
data Person = P
    { name :: String
    , address :: String
    , nationality :: String
    , age :: Int
    , numberOfChildren :: Int
    , father :: Person
    , mother :: Person
    }
~~~~

* Each field is a function

~~~~ {.haskell}
name :: Person -> String
P :: String -> String -> String -> Int -> Int ->
    Person -> Person -> Person
~~~~

# Type class Constraints (1)

~~~~ {.haskell}
data Container a =
    Empty
  | Holding a
~~~~

~~~~
> :t Holding 3
> Holding 'a' == Holding 'a'
> place Empty 'a'
~~~~~

# Type class Constraints (2)

* We must "enrol" `Container` into `Show` and `Eq`

~~~~ {.haskell}
class Show a where
    show :: a -> String
~~~~

~~~~ {.haskell}
class Eq a where 
    (==) :: a -> a -> Bool
~~~~

# Type class Constraints (3)

* Haskell can automatically derive most type class instances:

~~~~ {.haskell}
data Container a =
    Empty
  | Holding a
  deriving Show
~~~~

# Type class Constraints (4)

* Final definition:

~~~~ {.haskell}
data Container a =
    Empty
  | Holding a
  deriving (Show, Eq)
~~~~
