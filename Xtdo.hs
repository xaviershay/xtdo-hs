import System.Environment
import Data.Time.Calendar
import Data.List

-- 'main' runs the main program
main :: IO ()
main = do
  args <- getArgs
  finish (xtdo args [Task{ name="do something", scheduled=Nothing, category=Next}])

data TaskCategory = Today | Next | Scheduled deriving(Show, Eq)
data Task = Task { name :: String, scheduled :: Maybe Day, category :: TaskCategory } deriving(Show)

xtdo :: [String] -> [Task] -> ([Task], [TaskCategory])
xtdo ["l"]      tasks = (tasks, [Today])
xtdo ["l", "a"] tasks = (tasks, [Today, Next])

finish (tasks, categoriesToDisplay) = 
  putStrLn (intercalate "\n" (flatten(
  map formatCategoryGroup (groupByCategory categoriesToDisplay tasks))))

groupByCategory categories tasks = 
  map (\x -> (x, (filter (\task -> category task == x) tasks))) categories

flatten = foldl (++) [] -- Surely this is in the stdlib?

formatCategory :: TaskCategory -> String
formatCategory x = "==== " ++ show(x) ++ "\n"

formatTask :: Task -> String
formatTask x = "  " ++ name(x)

formatCategoryGroup = (\t -> [formatCategory(fst t)] ++ (map formatTask (snd t))) 
-- 
--
-- Each command returns:
--   The task data structure to be persisted
--   The types of tasks to be displayed
