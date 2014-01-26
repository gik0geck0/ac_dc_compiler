
import Debug.Trace
import Lexer
import Tokenizer

main :: IO ()
main = do
    s <- getLine
    putStrLn $ show $ parse $ tracer $ tokenize s
    main

tracer :: (Show a) => a -> a
tracer a = trace ("Tokens: " ++ show a) a
