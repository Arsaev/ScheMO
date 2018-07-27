module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool



symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


spaces :: Parser()
spaces = skipMany1 space


parseEscapedChar :: Parser String
parseEscapedChar = do
                     char '\\'
                     x <- oneOf "\"\\nrt"
                     return $ '\\' : [x]

parserCharToString :: Parser Char -> Parser String
parserCharToString c = c >>= return . (:"")

charToString :: Char -> String
charToString c = [c]

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (parseEscapedChar <|> unquoted)
                char '"'
                return $ String (concat x)
                where unquoted = parserCharToString (noneOf "\"")


parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many(letter <|> symbol <|> digit)
              let atom = first:rest
              return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= \x -> return $ Number (read x)

-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = do x <- many1 digit
--                  return $ Number (read x)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: "

main :: IO()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr) 

    -- args <- getArgs
    -- putStrLn ("Hello, " ++ show (read (args !! 0) + read (args !! 1)))
