import System.Environment

-- 'main' runs the main program
main :: IO ()
main = getArgs >>= print . xtdo . head

xtdo s = "Hello " ++ s
-- 
