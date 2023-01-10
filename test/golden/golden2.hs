module Golden2 where


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
  MoodReportH :: [(a,a)] -> Header a -- when writting the show instance, the strings should me mconcated with a newline charecter
  SleepH :: (a,a) -> Header a -- when writting the show instance, the strings should me mconcated with a newline charecter
  ProductivityH :: (a,a) -> Header a
  MeditationH :: [a] -> Header a
  AlcoholH :: (a,a) -> Header a
  CigaretteH :: (a,a,a) -> Header a
  RatingH :: a -> Header a
  AllHeaders :: [Header a] -> Header a


stringFloat :: GenParser Char st Char
stringFloat = digit <|> char '.'

-- instance Functor (Header a) where
--   fmap f ((l :: a -> Header a) a) = f a

instance (Show a) => Show (Header a) where
  show (NameH a)         = show a
  show (DateH a)         = show a
  show (MoodReportH a)   = show a
  show (SleepH a)        = show a
  show (ProductivityH a) = show a
  show (MeditationH a)   = show a
  show (AlcoholH a)      = show a
  show (CigaretteH a)    = show a
  show (RatingH a)       = show a
  show (AllHeaders a)    = show a

-- parses '\n' charecters
-- many newline :: GenParser Char st String
-- many newline = many (char '\n')

-- parses time in format of HH:MM
time :: GenParser Char st String
time = many1 digit <> many1 (char ':') <> many1 digit

-- computes the time of Sleep and Wake up
header :: String -> GenParser Char st String
header h = string $ mconcat ["[", h, "]"]

-- parses the name section
name :: GenParser Char st String
name = string "Name :"

-- parses the name section
parseName :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseName = do
  name
  spaces
  userName <- many1 alphaNum
  spaces
  userLName <- many1 alphaNum
  many newline
  return $ NameH (userName ++ " " ++  userLName)


-- parses the date section
date :: GenParser Char st String
date = string "Date :" <|> string "Date:"


dateSep :: GenParser Char st Char
dateSep = char '-' <|> char '/' <|> char '_' <|> char '\\'


-- parses the date section
-- parseDate :: forall a st. (a ~ String ) => GenParser Char st (Header a)
-- parseDate = do
--   date
--   spaces
--   userDate <- many1 (alphaNum <|> dateSep)
--   many newline
--   return $ Date userDate

parseDate :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseDate = do
  date
  spaces
  year <- many1 alphaNum
  dateSep
  month <- many1 alphaNum
  dateSep
  day <- many1 alphaNum
  many newline
  return $ DateH (year,month,day)

-- parses the mood
-- refactor with a list function

mood :: GenParser Char st String
mood = header "Mood"


-- Parses all the mood data constructors as stirngs
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



-- parses all the possible Intensities
parseIntensity :: GenParser Char st String
parseIntensity = choice $ map string ["Low", "Medium", "High", "Extreme"]

-- parses the sleep header
sleep :: GenParser Char st String
sleep = header "Sleep"


-- parses the sleep header and it's data
parseSleep :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseSleep = do
  sleep
  many newline
  (string "Wake up :" <* spaces) <|> (string "wake up :" <* spaces)
  wakeUpTime <- time
  many newline
  (string "Sleep :" <* spaces) <|> (string "sleep :" <* spaces)
  sleepTime <- time
  many newline
  return $ SleepH (wakeUpTime, sleepTime)


-- alcohol header
alcohol :: GenParser Char st String
alcohol = header "Alcohol"


-- parses the alcohol header and the data in it
parseAlcohol :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseAlcohol = do
  alcohol
  many newline
  drink <- (many1 alphaNum <* spaces) <* string ":"
  shots <- spaces *> many1 digit
  many newline
  return $ AlcoholH (drink,shots)


-- parses the cigarette header
cigarette :: GenParser Char st String
cigarette = header "Cigarette"


-- parses the cigarette header and the data in it
parseCigarette :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseCigarette = do
  cigarette
  many newline
  (string "Number :" <* spaces) <|> (string "number :" <* spaces)
  number <- many1 digit
  many newline
  (string "Nicotine :" <* spaces) <|> (string "nicotine :" <* spaces)
  nicotine <- many1 stringFloat
  many newline
  (string "Tar :" <* spaces) <|> (string "tar :" <* spaces)
  tar <- many1 stringFloat
  many newline
  return $ CigaretteH (number,nicotine,tar)


-- parses the meditation header
meditation :: GenParser Char st String
meditation = header "Meditation"

-- parses the meditatin header and the data in it
parseMeditations :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseMeditations = do
  meditation
  many newline
  meditations <- many1 (many1 (digit <|> char ':') <* many newline)
  many newline
  return $ MeditationH meditations


productivity :: GenParser Char st String
productivity = header "Productivity"

-- parses the productivity header and the information in it
parseProductivity :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseProductivity = do
  productivity
  many newline
  done <- many1 digit <* char '/'
  shouldHave <- many1 digit
  many newline
  return $ ProductivityH (done,shouldHave)


-- parses the rating header
rating :: GenParser Char st String
rating = header "Rating"


-- parses the different rating a user might give
parseRating' :: GenParser Char st String
parseRating' = choice $ map string ["Great", "Good", "Normal", "Bad", "Awful"]


-- parses the whole Rating header (section)
parseRating :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseRating = do
  rating
  many newline
  prsd <- parseRating'
  return $ RatingH prsd


-- parses the Entry written by the user (order of the entry doesn't matter)
parseEntry :: forall a st. (a ~ String) => GenParser Char st (Header a)
parseEntry = do
  entryParser <- many1 $ choice $ map try listOfParsers
  return . AllHeaders $ entryParser
 where listOfParsers = [ parseName, parseDate, parseMoodReports, parseSleep
                       , parseAlcohol, parseMeditations, parseCigarette
                       , parseProductivity, parseRating ]
