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
  template_name :: String,
  next          :: Day,
  period        :: String -- TODO: Better type definition
}
data ProgramData = ProgramData {
  tasks     :: [Task],
  recurring :: [RecurringTaskDefinition]
}
blankTask = Task{name="", scheduled=Nothing, category=Next}
data Formatter = PrettyFormatter | CompletionFormatter deriving(Show, Eq)

xtdo ["l"]      x _ = (tasks x, [Today], PrettyFormatter)
xtdo ["l", "a"] x _ = (tasks x, [Today, Next, Scheduled], PrettyFormatter)
xtdo ["l", "c"] x _ = (tasks x, [Today, Next, Scheduled], CompletionFormatter)

xtdo ("d":xs)   x _ = ([task | task <- tasks x,
                             hyphenize (name task) /= hyphenize (intercalate "-" xs)
                           ],
                           [Today, Next],
                           PrettyFormatter)
xtdo ("a":when:xs) x today
  | when =~ "0d?"               = (tasks x ++
                                   [makeTask xs (Just $ day today when) Today],
                                   [Today],
                                   PrettyFormatter)
  | when =~ "([0-9]+)([dwmy]?)" = (tasks x ++
                                   [makeTask xs (Just $ day today when) Scheduled],
                                   [Scheduled],
                                   PrettyFormatter)
  | otherwise                   = (tasks x ++
                                   [makeTask ([when] ++ xs) Nothing Next],
                                   [Next],
                                   PrettyFormatter)
  where
    makeTask n s c = blankTask{name=intercalate " " n,scheduled=s,category=c}

addCategory tasks today = map (addCategoryToTask today) tasks
  where
    addCategoryToTask today Task{name=n,scheduled=Just s}
      | s == today = blankTask{name=n,scheduled=Just s,category=Today}
      | otherwise  = blankTask{name=n,scheduled=Just s,category=Scheduled}

    addCategoryToTask today Task{name=n,scheduled=Nothing}
                 = blankTask{name=n,scheduled=Nothing,category=Next}


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

prettyFormatter (tasks, categoriesToDisplay) = do
  forM categoriesToDisplay (\currentCategory -> do
    putStrLn ""

    setSGR [ SetColor Foreground Dull Yellow ]
    putStrLn $ "==== " ++ show currentCategory
    putStrLn ""

    setSGR [Reset]
    forM [t | t <- tasks, category t == currentCategory] (\task -> do
      putStrLn $ "  " ++ name task
      )
    )
  putStrLn ""

completionFormatter (tasks, categoriesToDisplay) = do
  forM [t | t <- tasks] (\task -> do
    putStrLn $ hyphenize (name task)
    )
  putStr ""

hyphenize x = subRegex (mkRegex "[^a-zA-Z0-9]") x "-"

finish (tasks, categoriesToDisplay, formatter) = do
  encodeFile "tasks.yml" $ Mapping
    [ ("tasks", Sequence $ map toYaml tasks) ]
  doFormatting formatter (tasks, categoriesToDisplay)
  where doFormatting PrettyFormatter     = prettyFormatter
        doFormatting CompletionFormatter = completionFormatter
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
