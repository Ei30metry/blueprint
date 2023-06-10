# Blueprint-cli 
A Haskell tool for subsituting equivalent terms and pretty-printing the outgoing 
call hierarchy of GHC's Haskell AST.

## Introduction
The tool has two main commands:

- **show**: Pretty prints the outgoing call hierarchy.
- **substitute**: Substitutes equivalent terms in the Haskell AST.


## Usage 
`blueprint show f Main.hs` : definitin of function `f` in `Main.hs`.

`blueprint show f -l g Main.hs` : definition of local function `g`, in top-level function `f` located in `Main.hs`.

`blueprint show Name Main.hs` : definition of data type `Name` located in `Main.hs`.

`blueprint show Name -t Main.hs` : definition of data type `Name` with type synonyms converted to their original type located in `Main.hs`. 


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

### The `show` command 

`blueprint show "f'" Golden5.hs` would result in (this is the default output, called minimal):

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

### The `subsitute` command 

`blueprint substitute "f'" Golden5.hs`
would subsitute all the sub terms in the function `f` and print out the resulting functions: 

``` haskell
fSusbstituted = read . show . (+2)
```
### options and flags for `show`
- `-l func` or `--local-scope func` : Get the bluprint of a locally-scoped function.
- `-T` or `--type-signature` : Get the blueprint with type signatures. (if the function doesn't have a type signature, the inferred type will be printed.)


### Options and flags for `substitute`

- `-L` or `--Max-level` : Reduce Type-Synonyms to their original type.

## Universal Flags and Options

- `-c` or `--color`: whether the output should have syntax-highlighting or not. 
- `-L number` or `--level number` : Levels of implementation that should be pretty-printed. (default is all the way down to Prelude)
- `-h` or `--help` : The help screen.
- `-n` or `--line-number` : Print with line numbers.
- `-i FILE` or `--image FILE` : Save the result in an image file. (by default it prints to STDIO)
- `-j FILE` or `--json FILE` : Save the result in a JSON file. (by default it prints to STDIO)
- `-s FILE` or `--source-code`: Save the resulting haskell source to a file. (by default it prints to STDIO)
- `-m FILE` or `--minimal`: Print the resulting tree to a file. (by default it prints to STDIO)

