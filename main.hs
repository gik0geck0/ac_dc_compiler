
import Debug.Trace
import System.Console.Haskeline

import Lexer
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
            minput <- getInputLine "> "
            case minput of
                Nothing     -> return ()
                Just "quit" -> return ()
                Just input  -> do outputStrLn $ show $ parse $ tokenize input
                                  loop

tracer :: (Show a) => a -> a
tracer a = trace ("Tokens: " ++ show a) a
