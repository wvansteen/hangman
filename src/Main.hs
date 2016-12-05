module Main where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

data Puzzle = Puzzle String [Maybe Char] String String

instance Show Puzzle where
  show (Puzzle _ discovered guessed _) =
    intersperse ' ' (fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 4

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
    randomIndex <- randomRIO ( 0, length wl )
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) [] []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _ _) c = c `elem` w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g _) c = c `elem` g

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter p@(Puzzle word filledInSoFar s i) c =
  Puzzle word newFilledInSoFar (c:s) incorrect
  where
    incorrect = if charInWord p c then i else c:i
    zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar
    newFilledInSoFar =
      zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Youre guess was: " ++ [guess]
  case (charInWord puzzle guess
      , alreadyGuessed puzzle guess) of
    (_,True) -> do
      putStrLn "You already guessed that, \
              \ character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word,\
              \ filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
                \ the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ incorrect) =
  when (length incorrect > 7) $
    do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  when (all isJust filledInSoFar) $
    do
      putStrLn "You win!"
      exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $
  do
    gameWin puzzle
    gameOver puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _   -> putStrLn "Your guess must\
                      \ be a single character"
