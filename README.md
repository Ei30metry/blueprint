# Blueprint
A different approach to showing "outgoing call hierarchy" for Haskell source code.


## Introduction
Blueprint is a UNIX like command line utility which pretty-prints the definitions in outgoing call hierarchies by using the GHC Renamer to extract the definitions out of the AST. 

There are 2 commands in Blueprint:

- **function**
- **type**



## Usage 
`blueprint function f Main.hs` : definitin of function `f` in `Main.hs`.
`blueprint function f -l g Main.hs` : definition of local function `g`, in top-level function `f` located in `Main.hs`.
`blueprint type Name Main.hs` : definition of data type `Name` located in `Main.hs`.
`blueprint type Name -t Main.hs` : definition of data type `Name` with type synonyms converted to their original type located in `Main.hs`. 


## Example
Assume we want to see the "blueprint" of our `parseMoodReports` function defined below. (with path "HERE") 

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

Running `blueprint function "f'" Golden2.hs` would result in (this is the default minimal output):

``` text
(0) f' :=   (1) g'
            (2) f

(1) g' :=   (3) g

(2) f  :=   show 
            +

(3) g  :=   read
```

you can even give a root directory if you don't know the location of your entity.
`blueprint function f Main.hs` : definitin of function `f` in `Main.hs`.
`blueprint function f -l g Main.hs` : definition of local function `g`, in top-level function `f` located in `Main.hs`.
`blueprint type Name Main.hs` : definition of data type `Name` located in `Main.hs`.
`blueprint type Name -t Main.hs` : definition of data type `Name` with type synonyms converted to their original type located in `Main.hs`. 

## Installing pre-compiled binaries 
  * [ ] 
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
- `-i FILE` or `--image FILE` : Save the result in an image file.
- `-p FILE` or `--pdf FILE` : Save the result in a pdf file.
- `-s FILE` or `--source-code FILE` : save the result in a haskell file.
