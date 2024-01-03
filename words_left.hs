import System.Environment (getArgs)
import qualified Data.MultiSet as M

main = do
  (filename:guesses) <- getArgs
  words <- getWordsFromFile filename
  let result = wordsLeft words guesses
  mapM_ putStrLn result

getWordsFromFile :: String -> IO [String]
getWordsFromFile filename = do
  content <- readFile filename
  return (lines content)

wordsLeft :: [String] -> [String] -> [String]
wordsLeft words guesses = filter (matchesAll guesses) words

matchesAll :: [String] -> String -> Bool
matchesAll [] word = True
matchesAll (guess:score:other) word =
  matches guess score word && matchesAll other word

matches :: String -> String -> String -> Bool
matches guess score word =
     exactMatchesAreInPlace guess score word
  && misplacedMatchesAreNotInPlace guess score word
  && misplacedLettersArePresent guess score word
  && missedLettersAreAbsent guess score word

exactMatchesAreInPlace :: String -> String -> String -> Bool
exactMatchesAreInPlace guess score word =
  all id (zipWith3 exact guess score word)
  where exact g s w = if s == 'e' then g == w else True

misplacedMatchesAreNotInPlace :: String -> String -> String -> Bool
misplacedMatchesAreNotInPlace guess score word =
  all id (zipWith3 misplace guess score word)
  where misplace g s w = if s == 'm' then g /= w else True

misplacedLettersArePresent :: String -> String -> String -> Bool
misplacedLettersArePresent guess score word =
  let misplaced = getLettersScoring (=='m') guess score
  in (M.fromList misplaced) `M.isSubsetOf` (M.fromList word)

missedLettersAreAbsent :: String -> String -> String -> Bool
missedLettersAreAbsent guess score word =
  let missedLetters = M.fromList (getLettersScoring (=='x') guess score)
      matchedLetters = M.fromList (getLettersScoring (/='x') guess score)
      restOfWord = M.difference (M.fromList word) matchedLetters
  in M.null (M.intersection missedLetters restOfWord)

getLettersScoring :: (Char -> Bool) -> String -> String -> String
getLettersScoring f guess score =
  map fst $ filter (\(g,s)->f s) $ zip guess score
