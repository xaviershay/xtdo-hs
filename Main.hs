import Xtdo

import System.Environment
import Data.Time.Clock

main :: IO ()
main = do
  args <- getArgs
  tasks <- loadYaml
  now <- getCurrentTime
  let today = (utctDay now)
  finish $ xtdo args (addCategory tasks today) today
