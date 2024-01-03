import System.Environment (getArgs)
import Data.List (sort, nub, all)

main = do
  [filename, word] <- getArgs
  words <- getLinesFromFile filename
  let wordsUsing = filter (match word) words
  mapM_ putStrLn wordsUsing

getLinesFromFile :: String -> IO [String]
getLinesFromFile = fmap lines . readFile

match word dictWord =
  nub dictWord == dictWord
  && all (\c->elem c word) dictWord
