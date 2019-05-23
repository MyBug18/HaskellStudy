main :: IO ()
main = do
    strings <- readFile "./wordraw.txt" >>= return.lines
    writeFile "./wordsets.txt" $ unlines $ filter (\x -> length x > 3) strings