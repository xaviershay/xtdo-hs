import System.Environment
import Data.Time.Calendar
import Data.List

import Data.Object
import Data.Object.Yaml
import Control.Monad

-- 'main' runs the main program
main :: IO ()
main = do
  args <- getArgs
  tasks <- loadYaml
  finish $ xtdo args tasks

data TaskCategory = Today | Next | Scheduled deriving(Show, Eq)
data Task = Task { name :: String, scheduled :: Maybe Day, category :: TaskCategory } deriving(Show)

xtdo :: [String] -> [Task] -> ([Task], [TaskCategory])
xtdo ["l"]      tasks = (tasks, [Today])
xtdo ["l", "a"] tasks = (tasks, [Today, Next])

finish (tasks, categoriesToDisplay) = do
  encodeFile "tasks.yml" $ Sequence $ map toYaml tasks
  putStrLn $ intercalate "\n" output ++ "\n"
  where output = flatten [ [formatCategory c] ++ 
                           [formatTask t | t <- tasks, category t == c]
                         | c <- categoriesToDisplay]

flatten = foldl (++) [] -- Surely this is in the stdlib?

formatCategory :: TaskCategory -> String
formatCategory x = "\n==== " ++ show x ++ "\n"

formatTask :: Task -> String
formatTask x = "  " ++ name x

toYaml Task{name=x, scheduled=Nothing} = Mapping [("name", Scalar x)]

loadYaml = do
  object <- join $ decodeFile "tasks.yml"
  tasks <- fromSequence object >>= mapM extractTask
  return tasks

extractTask task = do
  m <- fromMapping task
  n <- lookupScalar "name" m
  return Task{name=n, scheduled=Nothing, category=Today}

testTasks = [Task{ name="do something", scheduled=Nothing, category=Today}]
-- 
--
-- Each command returns:
--   The task data structure to be persisted
--   The types of tasks to be displayed
