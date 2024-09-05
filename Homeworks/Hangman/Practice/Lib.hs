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
    show = printPuzzle

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
    
-- TODO
-- This function should generate initial random puzzle.
-- The part <Problem> should be random, so it has to be initiallized with the above function "getRandomWord".
-- The part <Status> should be filled with N-number of Nothing since we have no information,
--     where N denotes length of the problem string.
-- The part <Checked> should be an empty list since we have not tried anything.
getStartingPuzzle :: IO Puzzle
getStartingPuzzle = undefined

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    checkGameOver puzzle -- Checks if the game has over.
    checkGameWin puzzle  -- Checks if the user has won.
    print puzzle         -- Prints puzzle.
    input <- getOneInput -- Get one letter from user input.
    handleGuess puzzle input >>= runGame  -- Handles user input (Whether success or not), and restart the cycle.

-- TODO
-- This function should get one-letter input from user.
-- The "getLine" function is an IO action with the type of (IO String), and String is [Char],
--     so the input must be a single-lettered string such as ['a'], ['b'].
-- If the string contains more than one letter, or is not character, 
--     then it's an error, so you must handle it.
getOneInput :: IO Char
getOneInput = undefined

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle c = 
    case (alreadyTried puzzle c , isInProblem puzzle c) of
        -- If c is already checked, user must retry it.
        (True, _) -> putStrLn "You have already tried that character! Try again!" >> return puzzle
        -- If c is not checked, then it should be handled properly.
        (_, True) -> putStrLn "You got it!" >> return (fillGuess puzzle c)
        (_, False) -> putStrLn "You missed it!" >> return (fillGuess puzzle c)

-- TODO
-- This function handles user input.
-- If user input is in the problem, then modify <Status> properly, and put the input into <Checked>.
-- If not, then just put the input into <Checked>, without modifying <Status>.

-- EXAMPLE
-- let x1 = Puzzle "java" [Nothing, Nothing, Nothing, Nothing] [] -- Initial puzzle with the problem "java"
-- let x2 = fillGuess x1 'a'
-- x2 == Puzzle "java" [Nothing, Just 'a', Nothing, Just 'a'] ['a'] -- Checked 'a' successfully.
-- let x3 = fillGuess x2 'q'
-- x3 == Puzzle "java" [Nothing, Just 'a', Nothing, Just 'a'] ['q','a'] -- 'q' is not in <Problem>, so just added in <Checked>.
-- let x4 = fillGuess x3 'j'
-- x4 == Puzzle "java" [Just 'j', Just 'a', Nothing, Just 'a'] ['j', 'q', 'a'] -- Checked 'j' successfully.
fillGuess :: Puzzle -> Char -> Puzzle
fillGuess (Puzzle problem status checked) guess = undefined


-- These five functions are really easy, so I just implemented it.
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
