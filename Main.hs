import Xtdo

import System.Environment
import Data.Time.Clock

main :: IO ()
main = do
  args <- getArgs
  tasks <- loadYaml
  now <- getCurrentTime
  let today = (utctDay now)
  finish $ xtdo args (decoratedData tasks today) today
 -- ProgramData{tasks=tasks,recurring=recurring} =
  where decoratedData tasks today =
          ProgramData{tasks=decoratedTasks,recurring=[]}
          where decoratedTasks = addCategory tasks today
