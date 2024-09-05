import Control.Monad

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

type Variables = [(String, Maybe Int)]

getVar' :: String -> Variables -> Maybe Int
getVar' _ [] = Nothing
getVar' name ((n, v):ls) = if n == name then v else getVar' name ls

setVar' :: String -> Maybe Int -> Variables -> Variables
setVar' name val [] = [(name, val)]
setVar' name val ((n, v):ls) = if n == name then (n, val):ls else (n, v):(setVar' name val ls)

opVar' :: (Int -> Int -> Int) -> String -> String -> Variables -> Maybe Int
opVar' f n1 n2 env = do
    v1 <- getVar' n1 env
    v2 <- getVar' n2 env
    return $ f v1 v2

bad :: Variables -> (Maybe Int, Variables)
bad env0 = let env1 = setVar' "x" (Just 5) env0 in
   let env2 = setVar' "y" (Just 7) env1 in
       let val1 = opVar' (+) "x" "y" env2 in
           let env3 = setVar' "z" val1 env2 in
           (val1, env3)

data State s a = State { runState :: s -> (a, s) }

getVar :: String -> State Variables (Maybe Int)
getVar name = State $ \env -> (getVar' name env, env)

setVar :: String -> Maybe Int -> State Variables (Maybe Int)
setVar name val = State $ \env -> (val, setVar' name val env)

opVar :: (Int -> Int -> Int) -> String -> String -> State Variables (Maybe Int)
opVar f n1 n2 = State $ \env -> (opVar' f n1 n2 env, env)

good :: State Variables (Maybe Int)
good = State $ \env0 ->
    let (v1, env1) = (runState $ setVar "x" (Just 5)) env0 in
    let (v2, env2) = (runState $ setVar "y" (Just 7)) env1 in
    let (v3, env3) = (runState $ opVar (+) "x" "y") env2 in
        (runState $ setVar "z" v3) env3
 
instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State g) = State $ \s ->
        let (a, newState) = g s
        in (f a, newState)

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ \s -> (a, s)
    
    (<*>) :: State s (a -> b) -> State s a -> State s b
    (State sf) <*> (State sx) = State $ \s -> 
        let (f, newState) = sf s
            (x, finalState) = sx newState
        in (f x, finalState)

instance Monad (State s) where
    (>>=) :: State s a -> (a -> State s b) -> State s b
    (State x) >>= f = State (\s ->
        let (a, newState) = x(s)
        in runState (f a) newState)

better :: State Variables (Maybe Int)
better = setVar "x" (Just 5) >>= \v1 ->
    setVar "y" (Just 7) >>= \v2 ->
        opVar (+) "x" "y" >>= \v3 ->
            setVar "z" v3

best :: State Variables (Maybe Int)
best = setVar "x" (Just 5) >>
    setVar "y" (Just 7) >>
        opVar (+) "x" "y" >>= \v3 ->
            setVar "z" v3

perfect :: State Variables (Maybe Int)
perfect = do
    setVar "x" (Just 5)
    setVar "y" (Just 7)
    v3 <- opVar (+) "x" "y"
    setVar "z" v3
