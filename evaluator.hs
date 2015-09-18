import System.Environment
import Test.HUnit
import Data.Char (toLower)
import Data.Ratio
import Data.Complex
import Data.Array
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | Float Double
            | Rational Rational
            | Complex (Complex Float)
            | String String
            | Vector (Array Int LispVal)
            | Bool Bool
            | Char Char
            deriving (Eq)

parseExpr :: Parser LispVal
parseExpr = try parseRational
        <|> try parseComplex
        <|> try parseNumber
        <|> try parseDec
        <|> parseString
        <|> try parseChar
        <|> parseAtom
        <|> parseQuoted
        <|> parseVector
        <|> do
               char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x
        <|> parseQuasiQuote

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++")"
showVal (DottedList head tail) = "(" ++ unwordsList head
                                     ++  "."
                                     ++ showVal tail
                                     ++ ")"

instance Show LispVal where
    show = showVal

unwordsList = unwords . map showVal
readExpr :: String -> LispVal
readExpr input = case parse (parseExpr) "lisp" input  of
                   Left err -> String ("No match: " ++ (show err))
                   Right val -> val

parser rule text = parse rule "(source)" text

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

parseDec :: Parser LispVal
parseDec = string "#d" >> many (oneOf "0123456789.") >>= (\x -> return . Float . rf $ x)
  where
    rf = read :: String -> Double

parseComplex :: Parser LispVal
parseComplex =  do
    x <- many $ oneOf "0123456789."
    oneOf "+-"
    y <- many $ oneOf "0123456789."
    char 'i'
    return . Complex $ (read x :: Float) :+ (read y :: Float)

parseRational :: Parser LispVal
parseRational = do
    x <- many digit
    char '/'
    y <- many digit
    return . Rational $ (read x :: Integer) % (read y :: Integer)

parseChar :: Parser LispVal
parseChar = do
    string "#\\"
    x <- many1 letter
    return . Char $ case (map toLower x) of
            "newline" -> '\n'
            "space" -> ' '
            " " -> ' '
            [x] -> x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuote :: Parser LispVal
parseQuasiQuote = do
    char '`'
    char '('
    xs <- liftM List $ sepBy (try parseExpr <|> parseComma) spaces
    char ')'
    return $ List [Atom "quasiquote", xs]
  where
    parseComma :: Parser LispVal
    parseComma = do
                  char ','
                  x <- parseExpr
                  return $ List [Atom "unquote", x]

{-Come back later to this after reading about sets!-}
parseVector :: Parser LispVal
parseVector = do
  string "#("
  elems <- sepBy parseExpr spaces
  char ')'
  return . Vector $ listArray (0, (length elems) - 1 ) elems

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val

eval (List [Atom func, arg]) = maybe (Bool False) ($ arg) $ lookup func unaryOp
eval (List (Atom func: args)) = apply func $ map eval args

unaryOp :: [(String, LispVal -> LispVal)]
unaryOp = [("number?", isNumber),
           ("list?", isList),
           ("symbol?", isSymbol),
           ("string?", isString),
           ("boolean?", isBoolean),
           ("not", notOp),
           ("symbol->string", symbolToString),
           ("string->symbol", stringToSymbol)]

isNumber, isList, isSymbol, isString, isBoolean, notOp, symbolToString, stringToSymbol :: LispVal -> LispVal
isNumber (Number _) = Bool True
isNumber (_) = Bool False

isList (List _) = Bool True
isList (_) = Bool False

isSymbol (Atom _) = Bool True
isSymbol (List [Atom "quote", _]) = Bool True
isSymbol (_) = Bool False

isString (String _) = Bool True
isString (_) = Bool False

isBoolean (Bool _) = Bool True
isBoolean (_) = Bool False

notOp (Bool False) = Bool True
notOp (_) = Bool False

symbolToString (List [Atom "quote", x]) = x
stringToSymbol val@(String _) = List [Atom "quote", val]

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives:: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
             ("-", numericBinop (-)),
             ("*", numericBinop (*)),
             ("/", numericBinop div),
             ("mod", numericBinop mod),
             ("quotient", numericBinop quot),
             ("remainder", numericBinop rem)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in
  if null parsed
    then 1
    else fst $ parsed !! 0

unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

evaluator :: String -> LispVal
evaluator = eval . readExpr

main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)

testList =  TestList $ map TestCase [(assertEqual "" (Number 2) (evaluator "(+ 1 1)")),
                                    assertEqual "" (Number 3) (evaluator "(- (+ 4 6 3) 3 5 2)"),
                                    assertEqual "" (Bool True) (evaluator "(number? 123)"),
                                    assertEqual "" (Bool True) (evaluator "(string? \"hello\")"),
                                    assertEqual "Symbol type-testing" (Bool True) (evaluator "(symbol? 'hello)"),
                                    assertEqual "List type-testing" (Bool True) (evaluator "(list? (1 2 3))"),
                                    assertEqual "Symbol to string" (readExpr "flying-fish") (evaluator "(symbol->string 'flying-fish)")]
tests = runTestTT testList
