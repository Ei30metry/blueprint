# Blueprint
A different approach to showing "outgoing call hierarchy" for Haskell source code.


## Introduction
Blueprint is a UNIX like command line utility which pretty-prints the definitions in outgoing call hierarchies by using the GHC Renamer to extract the definitions out of the AST.

There are 2 commands in Blueprint:

- **function**
- **type**



## Usage 


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
BOOM!
.....
```

## Installing pre-compiled binaries 

## Building from source

### Dependencies
GHC (of course) 
stack 



## The `function` command 
Takes the function we want to pretty-print as an argument. 

### options and flags for `function`

- `-s func` `--scope func` : getting the bluprint of a locally-scoped function

## The `type` command 
takes the type we want to pretty-print as an argument.

### Options and flags for `type`

- `-t` or `--type-synonyms` : Reduce Type-Synonyms to their original type.

## Universal Flags and Options

- `-c` or `--color`: whether the output should have syntax-highlighting or not. 
- `-l number` or `--level number` : levels of implementation that should be pretty-printed. (default is all the way down to Prelude)
- `-h` or `--help` : The help screen.
- `-n` or `--line-number` : Print with line numbers


## Disclaimer
This program is designed to be used as a command-line utility and not as a library. For an integrated experience use the `call-hierarchy` plugin in [HLS](https://github.com/haskell/haskell-language-server).
