import System.Environment
import Data.Time.Calendar
import Data.List

-- 'main' runs the main program
main :: IO ()
main = do
  args <- getArgs
  finish $ xtdo args [Task{ name="do something", scheduled=Nothing, category=Today}]

data TaskCategory = Today | Next | Scheduled deriving(Show, Eq)
data Task = Task { name :: String, scheduled :: Maybe Day, category :: TaskCategory } deriving(Show)

xtdo :: [String] -> [Task] -> ([Task], [TaskCategory])
xtdo ["l"]      tasks = (tasks, [Today])
xtdo ["l", "a"] tasks = (tasks, [Today, Next])

finish (tasks, categoriesToDisplay) = 
  putStrLn $ intercalate "\n" output
  where output = flatten [ [formatCategory c] ++ 
                           [formatTask t | t <- tasks, category t == c] ++ 
                           [""]
                         | c <- categoriesToDisplay]

flatten = foldl (++) [] -- Surely this is in the stdlib?

formatCategory :: TaskCategory -> String
formatCategory x = "==== " ++ show x ++ "\n"

formatTask :: Task -> String
formatTask x = "  " ++ name x

-- 
--
-- Each command returns:
--   The task data structure to be persisted
--   The types of tasks to be displayed
