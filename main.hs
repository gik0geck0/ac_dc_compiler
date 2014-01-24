
import Lexer
import Tokenizer

main :: IO ()
main = do
    s <- getLine
    putStrLn $ show $ parse $ tokenize s
    main
