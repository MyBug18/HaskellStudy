import System.Random
-- About IO Monad

get2Input :: IO (String, String)
get2Input = do
    x <- getLine
    y <- getLine
    return (x, y)

get2Input' :: IO (String, String)
get2Input' = getLine >>= \x -> getLine >>= \y -> return (x, y)

terribleGet2Input :: (IO String, IO String)
terribleGet2Input = (getLine, getLine)

goodCat2Input :: IO String
goodCat2Input = get2Input >>= \(a, b) -> return (a ++ b)

terribleCat2Input :: IO String
terribleCat2Input =
    let (ioa, iob) = terribleGet2Input in
        ioa >>= \a -> iob >>= \b -> return (a ++ b)

badPrintAll :: Show a => [a] -> [IO ()]
badPrintAll = map print

printAll :: Show a => [a] -> IO ()
printAll [] = return ()
printAll (x:xs) = print x >> printAll xs

printAll' :: Show a => [a] -> IO [()]
printAll' [] = return []
printAll' (x:xs) = (:) <$> print x <*> printAll' xs

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ [] = return []
mapM' f (x:xs) = (:) <$> f x <*> mapM' f xs

mapM_' :: Monad m => (a -> m b) -> [a] -> m ()
mapM_' _ [] = return ()
mapM_' f (x:xs) = f x >> mapM_' f xs

when :: Monad m => Bool -> m () -> m ()
when False _ = return ()
when True x = x

whenEx :: IO ()
whenEx = do
    x <- getLine
    when (x == "asdf") $ putStrLn "fdsafdsa"
    when (x == "fdsa") $ putStrLn "asdfasdf"
    when True $ putStrLn "qwerqwer"

forever :: Monad m => m a -> m b
forever x = x >> forever x

foreverEx :: IO ()
foreverEx = forever $ do
    x <- getLine
    putStrLn $ "asdf" ++ x
    when (x == "asdf") $ return ()

-- About Random Values
getRandomNumber :: Int -> Int -> IO Int
getRandomNumber a b = randomRIO (a, b)

-- First example main function

main = forever $ do
    x <- getRandomNumber 1 9
    guess x where
        guess :: Int -> IO ()
        guess x = do
            input <- fmap read getLine
            if (input == x) 
                then putStrLn "Correct! Try again!"
                else putStrLn "Wrong!" >> guess x