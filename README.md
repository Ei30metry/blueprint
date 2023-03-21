# Blueprint
A different approach to showing "outgoing call hierarchy" for Haskell source code.


## Introduction
Blueprint is a UNIX like command line utility which pretty-prints the definitions in outgoing call hierarchies by using the GHC Renamer and Typechecker to extract the definitions out of the AST. 
The project's goal is to be used as a way of showing interactive substitution and inlining of expressions. 

There are 2 commands in Blueprint:

- **function**
- **type**


## Usage 
`blueprint function f Main.hs` : definitin of function `f` in `Main.hs`.

`blueprint function f -l g Main.hs` : definition of local function `g`, in top-level function `f` located in `Main.hs`.

`blueprint type Name Main.hs` : definition of data type `Name` located in `Main.hs`.

`blueprint type Name -t Main.hs` : definition of data type `Name` with type synonyms converted to their original type located in `Main.hs`. 


## Example
Assume we want to see the "blueprint" of our `f'` function defined below. (with path "HERE") 

```haskell 
module Golden5 where

import qualified Data.Text as T
import qualified Golden2 as G
import qualified Golden2 as GG
import qualified Golden6 as L

f :: Int -> String
f = g
  where g = show . (+2)


g :: String -> Int
g = read


pack :: String
pack = undefined


pack' :: String -> T.Text
pack' = T.pack

pack'' :: String
pack'' = G.pack

g' :: String -> Int
g' = g

g'' :: String -> Int
g'' = g'

f' :: Int -> Int
f' = g . f


l :: Int -> Int
l = g . f

test12 :: Int -> String
test12 = show . (+1) . id . f'


f'' :: Int -> String
f'' = f . f1
  where f1 = f'


test1 :: Int -> String
test1 = show . (+1) . id . f'

test1' :: String -> Int
test1' = (+2) . read . test1 . read

map' :: (a -> b) -> [a] -> [b]
map' = map


fPrime :: (Int -> String) -> (String -> Int) -> (Int -> Int)
fPrime f'' g = g . f''


fPrimeWithApplication :: (Int -> Int) -> Int
fPrimeWithApplication t = t 4

```

Running `blueprint function "f'" Golden2.hs` would result in (this is the default output, called minimal):

``` text
f'
|
+- .
|
+- f
|  |
|  +- .
|  |
|  +- +
|  |
|  `- show
|
`- g
   |
   `- read
```

## Installing pre-compiled binaries 

## Building from source

### Dependencies
GHC 
stack 


## The `function` command 
Takes the function we want to pretty-print as an argument. 

### options and flags for `function`

- `-l func` or `--local-scope func` : Get the bluprint of a locally-scoped function.
- `-T` or `--type-signature` : Get the blueprint with type signatures. (if the function doesn't have a type signature, the inferred type will be printed.)

## The `type` command 
takes the type we want to pretty-print as an argument.

### Options and flags for `type`

- `-t` or `--type-synonyms` : Reduce Type-Synonyms to their original type.

## Universal Flags and Options

- `-c` or `--color`: whether the output should have syntax-highlighting or not. 
- `-L number` or `--level number` : Levels of implementation that should be pretty-printed. (default is all the way down to Prelude)
- `-h` or `--help` : The help screen.
- `-n` or `--line-number` : Print with line numbers.
- `-i FILE` or `--image FILE` : Save the result in an image file. (by default it prints to STDIO)
- `-j FILE` or `--json FILE` : Save the result in a JSON file. (by default it prints to STDIO)
- `-s FILE` or `--source-code`: Save the resulting haskell source to a file. (by default it prints to STDIO)
- `-m FILE` or `--minimal`: Print the resulting tree to a file. (by default it prints to STDIO)

