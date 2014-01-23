
import Lexer
import Tokenizer

main :: IO ()
main = do
    s <- getLine
    putStrLn $ show $ lexer $ tokenize s
    main
