
import Data.Maybe
import Debug.Trace
import System.Console.Haskeline

import DCGen
import Parser
import Tokenizer

main :: IO ()
-- main = do
--     s <- getLine
--     putStrLn $ show $ parse $ tracer $ tokenize s
--     main
main = runInputT defaultSettings loop
    where
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine ""
            case minput of
                Nothing     -> return ()
                Just "quit" -> return ()
                Just input  -> 
                    let parseTree = parse $ tokenize input
                    in do
                        -- outputStrLn $ show parseTree
                        if isJust parseTree then
                            outputStrLn $ genDC $ fromJust parseTree
                        else outputStr ""
                        loop

tracer :: (Show a) => a -> a
tracer a = trace ("Tokens: " ++ show a) a
