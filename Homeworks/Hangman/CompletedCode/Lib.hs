module Lib where

import Control.Monad (forever)
import System.Random (randomRIO)
import System.Exit (exitSuccess)
import Data.Maybe (isJust)

type Problem = String -- The goal is to identify this string.
type Status = [Maybe Char] -- List of which characters are figured out.
type Checked = [Char] -- Characters users already tried.

data Puzzle = Puzzle Problem Status Checked deriving (Eq)

instance Show Puzzle where
    show p = printPuzzle p

printPuzzle :: Puzzle -> String
printPuzzle (Puzzle _ status checked) = 
    "Current status: " ++ concat (map printLetter status) ++ "Count: " ++ (show $ length checked) where
        printLetter :: Maybe Char -> String
        printLetter Nothing = "_ "
        printLetter (Just c) = c:" "

getRandomWord :: IO String
getRandomWord = do
    wordSet <- readFile "./wordcut/wordsets.txt" >>= return.lines
    randomIndex <- randomRIO (0, length wordSet - 1)
    return $ wordSet !! randomIndex
    
getStartingPuzzle :: IO Puzzle
getStartingPuzzle = getRandomWord >>= \x -> return $ Puzzle x (knowNothing $ length x) [] where
    knowNothing :: Int -> [Maybe Char]
    knowNothing l = take l $ repeat Nothing

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    checkGameOver puzzle
    checkGameWin puzzle
    print puzzle
    input <- getOneInput
    handleGuess puzzle input >>= runGame

getOneInput :: IO Char
getOneInput = do
    input <- getLine
    case input of
        x:[] -> if elem x ['a'..'z'] then return x else putStrLn "Invalid input. Try again." >> getOneInput
        _ -> putStrLn "Invalid input. Try again." >> getOneInput

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle c = 
    case (alreadyTried puzzle c , isInProblem puzzle c) of
        (True, _) -> putStrLn "You have already tried that character! Try again!" >> return puzzle
        (_, True) -> putStrLn "You got it!" >> return (fillGuess puzzle c)
        (_, False) -> putStrLn "You missed it!" >> return (fillGuess puzzle c)

fillGuess :: Puzzle -> Char -> Puzzle
fillGuess (Puzzle problem status checked) guess = 
    Puzzle problem (fillStatus problem status guess) (guess:checked) where
        fillStatus :: String -> [Maybe Char] -> Char -> [Maybe Char]
        fillStatus (p:ps) (s:ss) c = case s of
            Just x -> s:(fillStatus ps ss c)
            Nothing -> if c == p 
                then (Just c):(fillStatus ps ss c)
                else Nothing:(fillStatus ps ss c)
        fillStatus [] [] _ = []     

alreadyTried :: Puzzle -> Char -> Bool
alreadyTried (Puzzle _ _ checked) c = if elem c checked then True else False

isInProblem :: Puzzle -> Char -> Bool
isInProblem (Puzzle problem _ _) c = if elem c problem then True else False

checkGameOver :: Puzzle -> IO ()
checkGameOver (Puzzle problem _ checked) =
    if length checked > 20 
        then putStrLn ("You Lost! The original word was: " ++ problem) >> askContinue
        else return ()

checkGameWin :: Puzzle -> IO ()
checkGameWin (Puzzle problem status checked) =
    if all isJust status 
        then putStrLn ("You won with " ++ (show $ length checked) ++ " trials! The answer was \"" ++ problem ++ "\".") >> askContinue
        else return ()

askContinue :: IO ()
askContinue = do    
    putStrLn "Want to restart? (y/n)"
    input <- getLine
    case input of
        "y" -> putStrLn "Then restart!" >> getStartingPuzzle >>= runGame
        "n" -> exitSuccess
        _ -> putStrLn "Invalid input. Try again." >> askContinue
