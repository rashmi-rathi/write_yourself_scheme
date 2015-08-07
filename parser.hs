import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | String String
            | Bool Bool
            deriving (Show)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@i^_~#"

escapeChars = do
    char '\\'
    x <- oneOf ['"', '\\', 'n', 'r', 't']
    return $ (case x of
             '\\' -> '\\'
             '"' -> '"'
             'n'-> '\n'
             'r'-> '\r'
             't'-> '\t')

parseString :: Parser LispVal
parseString = do
            char '"'
            x <- many (escapeChars <|> noneOf "\"")
            char '"'
            return (String x)

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                    "#t" -> Bool True
                    "#f" -> Bool False
                    otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (liftM (Number . read) $ many1 digit ) <|> try parseHex <|> parseOct

parseHex :: Parser LispVal
parseHex = string "#x">> many (oneOf "0123456789abcdefABCDEF") >>= (\x -> return . Number . fst . head . readHex $ x)

parseOct :: Parser LispVal
parseOct = string "#o" >> many (oneOf "01234567") >>= (\x -> return . Number . fst . head . readOct $ x)

parseExpr :: Parser LispVal
parseExpr = parseNumber <|> parseString <|> parseAtom

readExpr :: String -> String
readExpr input = case parse (parseExpr) "lisp" input  of
                   Left err -> "No match: " ++ (show err)
                   Right val -> show val

parser rule text = parse rule "(source)" text

main :: IO()
main = do args <- getArgs
          putStrLn . readExpr $ args !! 0

