module Xtdo where
import System.Environment
import System.Console.ANSI

import Data.Time.Calendar
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
blankTask = Task{name="", scheduled=Nothing, category=Next}
data Formatter = PrettyFormatter     [TaskCategory] |
                 CompletionFormatter [TaskCategory] |
                 RecurringFormatter
                 deriving (Show, Eq)

data RecurInterval   = Daily | Weekly | Monthly | Yearly deriving (Show, Eq)
type RecurMultiplier = Int
type RecurOffset     = Int

data RecurFrequency = RecurFrequency RecurInterval RecurMultiplier RecurOffset deriving (Show, Eq)

xtdo :: [String] -> ProgramData -> Day -> (ProgramData, Formatter)
xtdo ["l"]      x _ = (x, PrettyFormatter [Today])
xtdo ["l", "a"] x _ = (x, PrettyFormatter [Today, Next, Scheduled])
xtdo ["l", "c"] x _ = (x, CompletionFormatter [Today, Next, Scheduled])
xtdo ["r", "l"] x _ = (x, RecurringFormatter)
xtdo ("r":"a":frequency:xs) x today =
  (addRecurring x (makeRecurring xs (parseFrequency frequency)), RecurringFormatter)
  where makeRecurring name frequency = RecurringTaskDefinition{
                                         frequency=frequency,
                                         templateName=intercalate " " name,
                                         nextOccurrence=(addDays 1 today)}
        addRecurring programData definition = ProgramData{
                                                tasks=tasks programData,
                                                recurring=(recurring programData) ++
                                                          [definition]}
xtdo ("d":xs)   x _ = (replaceTasks x [task | task <- tasks x,
                           hyphenize (name task) /= hyphenize (intercalate "-" xs)
                         ],
                         PrettyFormatter [Today, Next])
xtdo ("a":when:xs) x today
  | when =~ "0d?"               = (replaceTasks x (tasks x ++
                                   [makeTask xs (Just $ day today when) Today]),
                                   PrettyFormatter [Today])
  | when =~ "([0-9]+)([dwmy]?)" = (replaceTasks x (tasks x ++
                                   [makeTask xs (Just $ day today when) Scheduled]),
                                   PrettyFormatter [Scheduled])
  | otherwise                   = (replaceTasks x (tasks x ++
                                   [makeTask ([when] ++ xs) Nothing Next]),
                                   PrettyFormatter [Next])
  where
    makeTask n s c = blankTask{name=intercalate " " n,scheduled=s,category=c}
addCategory tasks today = map (addCategoryToTask today) tasks
  where
    addCategoryToTask today Task{name=n,scheduled=Just s}
      | s == today = blankTask{name=n,scheduled=Just s,category=Today}
      | otherwise  = blankTask{name=n,scheduled=Just s,category=Scheduled}

    addCategoryToTask today Task{name=n,scheduled=Nothing}
                 = blankTask{name=n,scheduled=Nothing,category=Next}


daysOfWeek = ["sun", "mon", "tue", "wed", "thu", "fri", "sat"]

parseFrequency :: String -> RecurFrequency
parseFrequency x = RecurFrequency interval multiplier (parseOffset offset)
  where matches = head $ (x =~ regex :: [[String]])
        multiplier = read (matches !! 1)
        interval   = charToInterval (matches !! 2)
        offset     = (matches !! 3)
        regex      = "([0-9]+)([dw]),?(" ++ (intercalate "|" daysOfWeek) ++ ")?"
        parseOffset :: String -> Int
        parseOffset x
          | x == ""             = 0
          | x `elem` daysOfWeek = length $ takeWhile (/= x) daysOfWeek
          | otherwise           = (read x :: Int)


        charToInterval :: String -> RecurInterval
        charToInterval "d" = Daily
        charToInterval "w" = Weekly

 --RecurFrequency Daily 1 0
replaceTasks x tasks = ProgramData{tasks=tasks,recurring=recurring x}

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

completionFormatter categoriesToDisplay programData = do
  forM [t | t <- tasks programData] (\task -> do
    putStrLn $ hyphenize (name task)
    )
  putStr ""

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

finish (programData, formatter) = do
  encodeFile "tasks.yml" $ Mapping
    [ ("tasks", Sequence $ map toYaml (tasks programData)) ]
  doFormatting formatter programData
  where doFormatting (PrettyFormatter x)     = prettyFormatter x
        doFormatting (CompletionFormatter x) = completionFormatter x
        doFormatting (RecurringFormatter   ) = recurringFormatter
        toYaml Task{name=x, scheduled=Nothing}   =
          Mapping [("name", Scalar x)]
        toYaml Task{name=x, scheduled=Just when} =
          Mapping [("name", Scalar x), ("scheduled", Scalar $ dayToString when)]
          where dayToString :: Day -> String
                dayToString = intercalate "-" . map show . toList . toGregorian
                  where toList (a,b,c) = [a, toInteger b, toInteger c]


flatten = foldl (++) [] -- Surely this is in the stdlib?

loadYaml = do
  object        <- join $ decodeFile "tasks.yml"
  mappings      <- fromMapping object
  tasksSequence <- lookupSequence "tasks" mappings
  tasks         <- mapM extractTask tasksSequence
  return tasks

extractTask task = do
  m <- fromMapping task
  n <- lookupScalar "name" m
  let s = lookupScalar "scheduled" m :: Maybe String
  return blankTask{name=n, scheduled=toDay s, category=Next}

toDay Nothing = Nothing
toDay (Just str) =
  Just $ fromGregorian (toInteger $ x!!0) (x!!1) (x!!2)
  where x = (map read $ splitOn "-" str :: [Int])
