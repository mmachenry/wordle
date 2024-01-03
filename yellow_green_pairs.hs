import System.Environment (getArgs)
import Data.List (sort)

main = do
  [filename] <- getArgs
  words <- getLinesFromFile filename
  let answer = yellowGreenPairs words
  mapM_ print answer
  print (length answer)
  print (sum (map (length.snd) answer))

getLinesFromFile :: String -> IO [String]
getLinesFromFile = fmap lines . readFile

yellowGreenPairs :: [String] -> [(String, [String])]
yellowGreenPairs [] = []
yellowGreenPairs (word:words) =
    let pairs = filter (isPair word) words
    in if length pairs > 0
       then (word,pairs) : yellowGreenPairs words
       else yellowGreenPairs words

isPair :: String -> String -> Bool
isPair w1 w2 = sort w1 == sort w2 && not (any id (zipWith (==) w1 w2))
