import System.Environment (getArgs)
import Data.List (sort)

main = do
  [filename, word] <- getArgs
  words <- getLinesFromFile filename
  let anagrams = filter (match word) words
  mapM_ putStrLn anagrams

getLinesFromFile :: String -> IO [String]
getLinesFromFile = fmap lines . readFile

match word other = sort word == sort other
