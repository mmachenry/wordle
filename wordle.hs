-- What is the most popular letter in the English language for a given
-- position in five letter words?
-- CARES (max position match?)
-- REAIS (max letter match and then optimize for position)

import qualified Data.Map as Map
import Data.Map (Map)
import System.Environment (getArgs)
import Data.Char (toUpper)
import Data.Ord (comparing)
import Data.List (sort, sortBy, nub)

wordLength = 5
type LetterCount = Map Char Int

main = do
  [filename] <- getArgs
  words <- getWordsFromFile filename
  let allResult = calculateLetterFrequency words
  putStrLn $ map fst $ reverse $ sortBy (comparing snd) $ Map.toList allResult
  let result = calculateLetterFrequencyByPos words
  mapM_ print (map (tabulate 26) result)

getWordsFromFile :: String -> IO [String]
getWordsFromFile filename = do
  content <- readFile filename
  return (lines content)

calculateLetterFrequency words = foldl countWord Map.empty words
  where countWord counts word =
          foldl increment counts (nub word)

calculateLetterFrequencyByPos words =
  let emptyCounts = replicate wordLength Map.empty
  in foldl addCounts emptyCounts words

tabulate n = take n . reverse . sortBy (comparing snd) . Map.toList

addCounts counts word =
  if length word == wordLength
  then zipWith increment counts (map toUpper word)
  else counts

increment :: LetterCount -> Char -> LetterCount
increment count char = Map.alter addOne char count
  where addOne Nothing = Just 1
        addOne (Just x) = Just (x + 1)

scorePositionMatches :: [LetterCount] -> String -> Int
scorePositionMatches counts word =
  sum $ zipWith (Map.findWithDefault 0) word counts

bestByPositionHitAlone words =
  let counts = calculateLetterFrequencyByPos words
      fiveLetterWords = filter (\w->length w == 5) words
  in take 10 $ reverse $ sortBy (comparing snd) $
     map (\w->(w,scorePositionMatches counts w)) fiveLetterWords

bestByMostLetterHitsThenPosition words =
  let allCounts = calculateLetterFrequency words
      posCounts = calculateLetterFrequencyByPos words
      top5Letters = map fst $ take 5 $ reverse $ sortBy (comparing snd) $ Map.toList allCounts
      wordsWithTop5 = filter (\w->sort w == sort top5Letters) words
  in reverse $ sortBy (comparing snd) $ map (\w->(w,scorePositionMatches posCounts w)) wordsWithTop5
