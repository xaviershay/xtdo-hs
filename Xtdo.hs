module Xtdo where
import System.Environment
import Data.Time.Calendar
import Data.Time.Clock
import Data.List
import Data.List.Split

import Data.Object
import Data.Object.Yaml
import Control.Monad

import Control.Failure

import Text.Regex.Posix

-- 'main' runs the main program
amain :: IO ()
amain = do
  args <- getArgs
  tasks <- loadYaml
  now <- getCurrentTime
  let today = (utctDay now)
  finish $ xtdo args (addCategory tasks today) today

data TaskCategory = Today | Next | Scheduled deriving(Show, Eq)
data Task = Task { name :: String, scheduled :: Maybe Day, category :: TaskCategory } deriving(Show)

addCategory tasks today = map (addCategoryToTask today) tasks
  where
    addCategoryToTask today Task{name=n,scheduled=Just s}
      | s == today = Task{name=n,scheduled=Just s,category=Today}
      | otherwise  = Task{name=n,scheduled=Just s,category=Scheduled}

    addCategoryToTask today Task{name=n,scheduled=Nothing} 
                 = Task{name=n,scheduled=Nothing,category=Next}


xtdo :: [String] -> [Task] -> Day -> ([Task], [TaskCategory])
xtdo ["l"]      tasks today = (tasks, [Today])
xtdo ["l", "a"] tasks today = (tasks, [Today, Next, Scheduled])
xtdo ("d":xs)   tasks today = ([x | x <- tasks, name x /= intercalate " " xs], [Today, Next])
xtdo ("a":when:xs) tasks today
  | when =~ "0d?"               = (tasks ++ [makeTask xs             (Just $ day today when) Today],     [Today])
  | when =~ "([0-9]+)([dwmy]?)" = (tasks ++ [makeTask xs             (Just $ day today when) Scheduled], [Scheduled])
  | otherwise                   = (tasks ++ [makeTask ([when] ++ xs) Nothing                 Next],      [Next])
  where
    makeTask n s c = Task{name=intercalate " " n,scheduled=s,category=c}

day :: Day -> String -> Day
day today when = modifier today
  where   matches  = head $ (when =~ "([0-9]+)([dwmy]?)" :: [[String]])
          offset   = read $ (matches !! 1)
          modifier = charToModifier (matches !! 2) offset

          -- Converts a char into a function that will transform a date by the given offset
          charToModifier :: String -> (Integer -> Day -> Day)
          charToModifier "d" = addDays
          charToModifier "w" = addDays . (* 7)
          charToModifier "m" = addGregorianMonthsClip
          charToModifier "y" = addGregorianYearsClip
          charToModifier other = error other


finish (tasks, categoriesToDisplay) = do
  encodeFile "tasks.yml" $ Sequence $ map toYaml tasks
  putStrLn $ intercalate "\n" output ++ "\n"
  where output = flatten [ [formatCategory c] ++ 
                           [formatTask t | t <- tasks, category t == c]
                         | c <- categoriesToDisplay]
        formatCategory :: TaskCategory -> String
        formatCategory x = "\n==== " ++ show x ++ "\n"

        formatTask :: Task -> String
        formatTask x = "  " ++ name x

        toYaml Task{name=x, scheduled=Nothing}   = 
          Mapping [("name", Scalar x)]
        toYaml Task{name=x, scheduled=Just when} = 
          Mapping [("name", Scalar x), ("scheduled", Scalar $ dayToString when)]
          where dayToString :: Day -> String
                dayToString = intercalate "-" . map show . toList . toGregorian
                  where toList (a,b,c) = [a, toInteger b, toInteger c]


flatten = foldl (++) [] -- Surely this is in the stdlib?

loadYaml = do
  object <- join $ decodeFile "tasks.yml"
  tasks <- fromSequence object >>= mapM extractTask
  return tasks

extractTask task = do
  m <- fromMapping task
  n <- lookupScalar "name" m
  let s = lookupScalar "scheduled" m :: Maybe String
  return Task{name=n, scheduled=toDay s, category=Next}

toDay Nothing = Nothing
toDay (Just str) = 
  Just $ fromGregorian (toInteger $ x!!0) (x!!1) (x!!2)
  where x = (map read $ splitOn "-" str :: [Int])

testTasks = [Task{ name="do something", scheduled=Nothing, category=Today}]
-- 
--
-- Each command returns:
--   The task data structure to be persisted
--   The types of tasks to be displayed
