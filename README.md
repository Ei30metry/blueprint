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
module Parser.Input(Header (..),
                   parseEntry
                   ) where

import           Data.List                     ( sortOn )
import           Data.Singletons.Base.TH       ( singletons )

import           Text.Parsec                   ( alphaNum )
import           Text.Parsec.Char              ( newline )
import           Text.ParserCombinators.Parsec ( GenParser, alphaNum, char,
                                                 choice, digit, many, many1,
                                                 sepBy, spaces, string, try,
                                                 (<|>) )


data Header a where
  NameH :: a -> Header a
  DateH :: (a,a,a) -> Header a
  MoodReportH :: [(a,a)] -> Header a 
  SleepH :: (a,a) -> Header a 
  ProductivityH :: (a,a) -> Header a
  MeditationH :: [a] -> Header a
  AlcoholH :: (a,a) -> Header a
  CigaretteH :: (a,a,a) -> Header a
  RatingH :: a -> Header a
  AllHeaders :: [Header a] -> Header a


parseMood :: GenParser Char st String
parseMood = choice $ map string ["Neutral", "Angry", "Sad"
                                 ,"Excited", "Happy", "Focused", "Bored"]


-- parses one mood
parseMoodReport :: GenParser Char st (String, String)
parseMoodReport = do
  userMood <- parseMood
  spaces
  char ':'
  spaces
  moodIntensity <- parseIntensity
  many newline
  return (userMood, moodIntensity)


parseMoodReports :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseMoodReports = do
  mood
  many newline
  l <- many1 parseMoodReport <* many newline
  return $ MoodReportH $ sortOn fst l
```

Running `blueprint function parseMoodReports -c "HERE"` would result in:

``` text
parseMoodReports := mood, many, newline, many1, parseMoodReport, (<*), (>>), (>>=), return, sortOn, fst
... 
... 
... 
.....
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
