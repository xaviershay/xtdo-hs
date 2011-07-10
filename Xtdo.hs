-- FlexibleContexts needed for explicit type declarations on YAML functions
{-# LANGUAGE FlexibleContexts #-}

module Xtdo where
import System.Environment
import System.Console.ANSI

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.List
import Data.List.Split

import Data.Object
import Data.Object.Yaml
import Control.Monad

import Control.Failure

import Text.Regex.Posix
import Text.Regex(subRegex, mkRegex)

data TaskCategory = Today | Next | Scheduled deriving(Show, Eq)

data Task = Task {
  name      :: String,
  scheduled :: Maybe Day,
  category  :: TaskCategory
} deriving(Show, Eq)
blankTask = Task{name="", scheduled=Nothing, category=Next}

data RecurringTaskDefinition = RecurringTaskDefinition {
  -- ideally this would be 'name' but haskell doesn't like the collision with
  -- Task.name
  templateName   :: String,
  nextOccurrence :: Day,
  frequency      :: RecurFrequency
} deriving (Show, Eq)

data ProgramData = ProgramData {
  tasks     :: [Task],
  recurring :: [RecurringTaskDefinition]
} deriving (Show, Eq)

data Formatter = PrettyFormatter     [TaskCategory] |
                 CompletionFormatter [TaskCategory] |
                 RecurringFormatter
                 deriving (Show, Eq)

data DayInterval     = Day | Week | Month | Year deriving (Show, Eq)
type RecurMultiplier = Int
type RecurOffset     = Int

data RecurFrequency = RecurFrequency DayInterval RecurMultiplier RecurOffset deriving (Show, Eq)

xtdo :: [String] -> ProgramData -> Day -> (ProgramData, Formatter)
xtdo args programData today = (createRecurring today $ fst result, snd result)
  where result = xtdo' args programData today

xtdo' :: [String] -> ProgramData -> Day -> (ProgramData, Formatter)
xtdo' ["l"]      x t = (x, PrettyFormatter [Today])
xtdo' ["l", "a"] x t = (x, PrettyFormatter [Today, Next, Scheduled])
xtdo' ["l", "c"] x t = (x, CompletionFormatter [Today, Next, Scheduled])
xtdo' ["r", "l"] x _ = (x, RecurringFormatter)
xtdo' ("r":"a":frequencyString:xs) x today =
  (addRecurring x makeRecurring, RecurringFormatter)
  where makeRecurring =
          RecurringTaskDefinition{
            frequency      = frequency,
            templateName   = name,
            nextOccurrence = nextOccurrence
          }
        name           = intercalate " " xs
        frequency      = parseFrequency frequencyString
        nextOccurrence = calculateNextOccurrence today frequency

xtdo' ("d":xs)   x t = (replaceTasks x [task | task <- tasks x,
                           hyphenize (name task) /= hyphenize (intercalate "-" xs)
                         ],
                         PrettyFormatter [Today, Next])

xtdo' ("b":when:xs) x today
  | when =~ "([0-9]+)([dwmy]?)" =
    run taskToBump
    where taskToBump      = find ((==) taskNameToBump . hyphenize . name) (tasks x)
          taskNameToBump  = hyphenize $ intercalate " " xs
          parsedDay       = day today when
          wrapData x      = (x, PrettyFormatter [Today, Next, Scheduled])
          run Nothing     = wrapData x
          run (Just task) = wrapData $ replaceTasks x $
                              task{
                                category  = categoryForScheduled
                                              today
                                              (Just parsedDay),
                                scheduled = Just parsedDay
                              }:(delete task (tasks x))

xtdo' ("a":when:xs) x today
  | when =~ "([0-9]+)([dwmy]?)" = run (Just parsedDay) xs
  | otherwise                   = run Nothing          (when:xs)
  where
    parsedDay = day today when

    run scheduled name = ( addTask x blankTask{
                             name      = intercalate " " name,
                             scheduled = scheduled,
                             category  = categoryForScheduled today scheduled
                           }
                         , PrettyFormatter [categoryForScheduled today scheduled]
                         )

addCategory tasks today = map (addCategoryToTask today) tasks
  where
    addCategoryToTask today Task{name=n,scheduled=Just s}
      | s == today = blankTask{name=n,scheduled=Just s,category=Today}
      | otherwise  = blankTask{name=n,scheduled=Just s,category=Scheduled}

    addCategoryToTask today Task{name=n,scheduled=Nothing}
                 = blankTask{name=n,scheduled=Nothing,category=Next}

createRecurring :: Day -> ProgramData -> ProgramData
createRecurring today programData =
  replaceRecurring (foldl addTask programData noDuplicatedTasks) newRecurring
  where matching = filter (\x -> nextOccurrence x <= today) (recurring programData)
        newRecurring =
          map (recalculateNextOccurrence today) (recurring programData)
        recalculateNextOccurrence
          :: Day -> RecurringTaskDefinition -> RecurringTaskDefinition
        recalculateNextOccurrence today definition
          | nextOccurrence definition <= today = definition{
              nextOccurrence = calculateNextOccurrence today (frequency definition)
            }
          | otherwise                          = definition

        newtasks =
          map taskFromRecurDefintion matching
        noDuplicatedTasks =
          filter notInExisting newtasks
        notInExisting :: Task -> Bool
        notInExisting task =
          (hyphenize . name $ task) `notElem` existingTaskNames
        existingTaskNames =
          map (hyphenize . name) (tasks programData)
        taskFromRecurDefintion x =
          blankTask {name=templateName x,scheduled=Just today,category=Today}

daysOfWeek = ["sun", "mon", "tue", "wed", "thu", "fri", "sat"]

parseFrequency :: String -> RecurFrequency
parseFrequency x =
  RecurFrequency interval multiplier (parseOffset offset)
  where matches = head $ (x =~ regex :: [[String]])
        multiplier = read (matches !! 1)
        interval   = charToInterval (matches !! 2)
        offset     = (matches !! 3)
        regex      = "([0-9]+)([dwmy]),?([0-9]+|" ++ (intercalate "|" daysOfWeek) ++ ")?"
        parseOffset :: String -> Int
        parseOffset x
          | x == ""             = 0
          | x `elem` daysOfWeek = length $ takeWhile (/= x) daysOfWeek
          | otherwise           = (read x :: Int) - 1


        charToInterval :: String -> DayInterval
        charToInterval "d" = Day
        charToInterval "w" = Week
        charToInterval "m" = Month
        charToInterval "y" = Year


data StepDirection = Forward | Backward

-- This method works by build up an infinite sequence of days on which
-- the definition will occur starting from a day earlier than today, then
-- picking the first element in the sequence that is greater than today.
calculateNextOccurrence :: Day -> RecurFrequency -> Day
calculateNextOccurrence today (RecurFrequency interval multiplier offset) =
  head $ dropWhile (<= today) (occurencesFrom firstOccurrence)
  where firstOccurrence        = addDays
                                  (toInteger offset)
                                  (startOfInterval interval startDay)
        occurencesFrom day     = day:(occurencesFrom $ stepByInterval day Forward)
        startDay               = stepByInterval today Backward
        stepByInterval day dir =
          (intervalToModifier interval)
          (toInteger multiplier * (modifier dir))
          day
          where modifier Forward  = 1
                modifier Backward = -1

-- These date functions complement those provided by the standard library.

startOfInterval Day   day = day
startOfInterval Week  day = fromSundayStartWeek (year day) (week day) 0
startOfInterval Month day = fromGregorian (year day) (month day) 1
startOfInterval Year  day = fromGregorian (year day) 1 1

year :: Day -> Integer
year  = fst . toOrdinalDate

month :: Day -> Int
month = month' . toGregorian
  where month' (_, x, _) = x

week :: Day -> Int
week  = fst . sundayStartWeek

-- Functions to manipulate ProgramData
replaceTasks :: ProgramData -> [Task] -> ProgramData
replaceTasks x tasks = ProgramData{tasks=tasks,recurring=recurring x}

replaceRecurring :: ProgramData -> [RecurringTaskDefinition] -> ProgramData
replaceRecurring x recurring = ProgramData{tasks=tasks x,recurring=recurring}

addTask :: ProgramData -> Task -> ProgramData
addTask programData task =
  ProgramData{
    tasks     = task:(tasks programData),
    recurring = (recurring programData)
  }

addRecurring :: ProgramData -> RecurringTaskDefinition -> ProgramData
addRecurring programData definition =
  ProgramData{
    tasks     = tasks programData,
    recurring = definition:(recurring programData)
  }

day :: Day -> String -> Day
day today when = modifier today
  where   matches  = head $ (when =~ "([0-9]+)([dwmy]?)" :: [[String]])
          offset   = read $ (matches !! 1)
          modifier = charToModifier (matches !! 2) offset

          -- Converts a char into a function that will transform a date
          -- by the given offset
          charToModifier :: String -> (Integer -> Day -> Day)
          charToModifier ""  = addDays
          charToModifier "d" = addDays
          charToModifier "w" = addDays . (* 7)
          charToModifier "m" = addGregorianMonthsClip
          charToModifier "y" = addGregorianYearsClip
          charToModifier other = error other

categoryForScheduled today Nothing = Next
categoryForScheduled today (Just day)
  | day == today = Today
  | otherwise    = Scheduled

intervalToModifier :: DayInterval -> (Integer -> Day -> Day)
intervalToModifier Day = addDays
intervalToModifier Week = addDays . (* 7)
intervalToModifier Month = addGregorianMonthsClip
intervalToModifier Year  = addGregorianYearsClip

prettyFormatter :: [TaskCategory] -> ProgramData -> IO ()
prettyFormatter categoriesToDisplay programData = do
  forM categoriesToDisplay (\currentCategory -> do
    putStrLn ""

    setSGR [ SetColor Foreground Dull Yellow ]
    putStrLn $ "==== " ++ show currentCategory
    putStrLn ""

    setSGR [Reset]
    forM [t | t <- tasks programData, category t == currentCategory] (\task -> do
      putStrLn $ "  " ++ name task
      )
    )
  putStrLn ""

completionFormatter :: [TaskCategory] -> ProgramData -> IO ()
completionFormatter categoriesToDisplay programData = do
  forM [t | t <- tasks programData] (\task -> do
    putStrLn $ hyphenize (name task)
    )
  putStr ""

recurringFormatter :: ProgramData -> IO ()
recurringFormatter programData = do
  putStrLn ""

  setSGR [ SetColor Foreground Dull Yellow ]
  putStrLn $ "==== Recurring"
  putStrLn ""

  setSGR [Reset]
  forM (recurring programData) (\definition -> do
    putStrLn $ "  " ++ templateName definition
    )
  putStrLn ""

hyphenize x = subRegex (mkRegex "[^a-zA-Z0-9]") x "-"

finish :: (ProgramData, Formatter) -> IO ()
finish (programData, formatter) = do
  encodeFile "tasks.yml" $ Mapping
    [ ("tasks", Sequence $ map toYaml (tasks programData))
    , ("recurring", Sequence $ map recurToYaml (recurring programData))]
  doFormatting formatter programData
  where doFormatting (PrettyFormatter x)     = prettyFormatter x
        doFormatting (CompletionFormatter x) = completionFormatter x
        doFormatting (RecurringFormatter   ) = recurringFormatter
        recurToYaml x =
          Mapping [ ("templateName",   Scalar (templateName x))
                  , ("nextOccurrence", Scalar (dayToString       $ nextOccurrence x))
                  , ("frequency",      Scalar (frequencyToString $ frequency x))
                  ]
        toYaml Task{name=x, scheduled=Nothing}   =
          Mapping [("name", Scalar x)]
        toYaml Task{name=x, scheduled=Just when} =
          Mapping [("name", Scalar x), ("scheduled", Scalar $ dayToString when)]


dayToString :: Day -> String
dayToString = intercalate "-" . map show . toList . toGregorian
  where toList (a,b,c) = [a, toInteger b, toInteger c]

frequencyToString :: RecurFrequency -> String
frequencyToString x = "1d"

flatten = foldl (++) [] -- Surely this is in the stdlib?

loadYaml :: IO ProgramData
loadYaml = do
  object        <- join $ decodeFile "tasks.yml"
  mappings      <- fromMapping object
  tasksSequence <- lookupSequence "tasks" mappings
  tasks         <- mapM extractTask tasksSequence
  recurSequence <- lookupSequence "recurring" mappings
  recurring     <- mapM extractRecurring recurSequence
  return ProgramData {tasks=tasks,recurring=recurring}

extractRecurring
  :: (Failure ObjectExtractError m) =>
     StringObject -> m RecurringTaskDefinition
extractRecurring x = do
  m <- fromMapping x
  n <- lookupScalar "templateName"   m
  d <- lookupScalar "nextOccurrence" m
  f <- lookupScalar "frequency"      m
  return RecurringTaskDefinition{
      templateName   = n,
      nextOccurrence = parseDay       d,
      frequency      = parseFrequency f
    }

parseDay :: String -> Day
parseDay x =
  unwrapDay (toDay $ Just x)
  where unwrapDay :: Maybe Day -> Day
        unwrapDay Nothing  = error x
        unwrapDay (Just x) = x


extractTask
  :: (Failure ObjectExtractError m) => StringObject -> m Task
extractTask task = do
  m <- fromMapping task
  n <- lookupScalar "name" m
  let s = lookupScalar "scheduled" m :: Maybe String
  return blankTask{name=n, scheduled=toDay s, category=Next}

toDay :: Maybe String -> Maybe Day
toDay Nothing = Nothing
toDay (Just str) =
  Just $ fromGregorian (toInteger $ x!!0) (x!!1) (x!!2)
  where x = (map read $ splitOn "-" str :: [Int])
