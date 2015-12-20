{-# LANGUAGE ExistentialQuantification #-}
import System.Environment
import Test.HUnit
import Data.Char (toLower)
import Data.Ratio
import Data.Complex
import Data.Array
import Control.Monad
import Control.Monad.Error
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

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values" ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected" ++ expected ++ ", found" ++ show found
showError (Parser parseErr) = "Parse error at" ++ show parseErr

instance Show LispError where
  show = showError

instance Error LispError where
  noMsg = Default "An error has occured"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

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
showVal (Char char) = char:[]
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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

readExpr :: String -> ThrowsError LispVal
readExpr input =
  case parse (parseExpr) "lisp" input  of
    Left err -> throwError $ Parser err
    Right val -> return val


spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@i^_~#"

escapeChars =
  do
    char '\\'
    x <- oneOf ['"', '\\', 'n', 'r', 't']
    return $ (case x of
             '\\' -> '\\'
             '"' -> '"'
             'n'-> '\n'
             'r'-> '\r'
             't'-> '\t')

parseString :: Parser LispVal
parseString =
  do
    char '"'
    x <- many (escapeChars <|> noneOf "\"")
    char '"'
    return (String x)

parseAtom :: Parser LispVal
parseAtom =
  do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = [first] ++ rest
    return $ case atom of
         "#t" -> Bool True
         "#f" -> Bool False
         otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (liftM (Number . read) $ many1 digit ) <|> try parseHex <|> parseOct

parseHex :: Parser LispVal
parseHex = string "#x">> many (oneOf "0123456789abcdefABCDEF") >>= return . Number . fst . head . readHex

parseOct :: Parser LispVal
parseOct = string "#o" >> many (oneOf "01234567") >>= return . Number . fst . head . readOct

parseDec :: Parser LispVal
parseDec = string "#d" >> many (oneOf "0123456789.") >>= return . Float . rf
  where
    rf = read :: String -> Double

parseComplex :: Parser LispVal
parseComplex =
  do
    x <- many $ oneOf "0123456789."
    oneOf "+-"
    y <- many $ oneOf "0123456789."
    char 'i'
    return . Complex $ (read x :: Float) :+ (read y :: Float)

parseRational :: Parser LispVal
parseRational =
  do
    x <- many digit
    char '/'
    y <- many digit
    return . Rational $ (read x :: Integer) % (read y :: Integer)

parseChar :: Parser LispVal
parseChar =
  do
    string "#\\"
    x <- many1 letter
    return . Char $ case (map toLower x) of
            "newline" -> '\n'
            "space" -> ' '
            " " -> ' '
            [y] -> head x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList =
  do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted =
  do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuote :: Parser LispVal
parseQuasiQuote =
  do
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
parseVector =
  do
    string "#("
    elems <- sepBy parseExpr spaces
    char ')'
    return . Vector $ listArray (0, (length elems) - 1 ) elems

eval :: LispVal -> ThrowsError LispVal
eval val@(Char _) = return val
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Atom "else") = return $ Bool True
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
  do
    result <- eval pred
    x <- ifElse result conseq alt
    return x
    where
      ifElse pred conseq alt
        | (pred == Bool True) = (eval conseq)
        | (pred == Bool False) = (eval alt)

eval (List (Atom "cond": args)) = cond args
eval (List (Atom "case" : key : clauses )) =
  do
    evalKey <- eval key
    result <- let
                memv el (List xs) =  liftM (any (== Bool True)) . sequence $ (\x -> eqv [el, x]) <$> xs
                compareDatum key (List (datum:_)) = memv key datum
              in
                case (last clauses) of
                  val@(List [Atom "else", key]) -> case (liftM null $ clausesWithoutElse) of
                                                     Right True -> Right [val]
                                                     _ -> filterM (compareDatum evalKey) (init clauses)
                                                     where
                                                        clausesWithoutElse = filterM (compareDatum evalKey) (init clauses)
                  _ -> filterM (compareDatum evalKey) clauses
    let List (datum:ckey:[]) = head result in (eval ckey)

eval (List (Atom func: args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

isNumber, isList, isSymbol, isString, isBoolean, notOp, symbolToString, stringToSymbol :: LispVal -> ThrowsError LispVal
isNumber (Number _) = return $ Bool True
isNumber (_) = return $ Bool False

isList (List _) = return $ Bool True
isList (_) = return $ Bool False

isSymbol (Atom _) = return $ Bool True
isSymbol (List [Atom "quote", _]) = return $ Bool True
isSymbol (_) = return $ Bool False

isString (String _) = return $ Bool True
isString (_) = return $ Bool False

isBoolean (Bool _) = return $ Bool True
isBoolean (_) = return $ Bool False

notOp (Bool False) = return $ Bool True
notOp (_) = return $ Bool False

symbolToString (List [Atom "quote", x]) = return $ x
symbolToString val@(_) = return val
stringToSymbol val@(String _) = return $ List [Atom "quote", val]

makeString, stringLength, stringRef, substring, stringAppend :: [LispVal] -> ThrowsError LispVal
makeString [Number n] = return $ String $ replicate (fromIntegral n) ' '
makeString [Number n, Char x] = return $ String $ replicate (fromIntegral n) x
stringLength [String str] = return . Number .fromIntegral $ length str
stringRef [String str, Number n] = return . Char $ str !! (fromIntegral n)
substring [String str, Number n, Number m] = return . String $  take mi $ drop ni str
  where
    [mi, ni] = map fromIntegral [m-1,n]
stringAppend strList = liftM String $ liftM (foldr1 (++)) . sequence $ unpackStr <$> strList
joinString str = liftM String . sequence $ unpackChar <$> str

stringToList, listToString :: [LispVal] -> ThrowsError LispVal
stringToList [String str] = return . List $ map Char str
listToString [List listOfChars] = String <$> mapM unpackChar listOfChars

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) $ lookup func primitives

primitives:: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
             ("-", numericBinop (-)),
             ("*", numericBinop (*)),
             ("/", numericBinop div),
             ("mod", numericBinop mod),
             ("quotient", numericBinop quot),
             ("remainder", numericBinop rem),
             ("=", numBoolBinop (==)),
             ("<", numBoolBinop (<)),
             (">", numBoolBinop (>)),
             (">=", numBoolBinop (>=)),
             ("<=", numBoolBinop (<=)),
             ("&&", boolBoolBinop (&&)),
             ("||", boolBoolBinop (||)),

             ("string=?", strBoolBinop(==)),
             ("string<?", strBoolBinop (<)),
             ("string>?", strBoolBinop (>)),
             ("string<=?", strBoolBinop (<=)),
             ("string>=?", strBoolBinop (>=)),
             ("make-string", makeString),
             ("string-length", stringLength),
             ("string-ref", stringRef),
             ("substring", substring),
             ("string-append", stringAppend),
             ("string", joinString),
             ("string->list", stringToList),
             ("list->string", listToString),


             ("number?", unaryOperator isNumber),
             ("list?", unaryOperator isList),
             ("symbol?", unaryOperator isSymbol),
             ("string?", unaryOperator isString),
             ("boolean?", unaryOperator isBoolean),

             ("not", unaryOperator notOp),
             ("symbol->string", unaryOperator symbolToString),
             ("string->symbol", unaryOperator stringToSymbol),
             ("eq?", eqv),
             ("eqv?", eqv),
             ("equal?", equal),
             ("cons", cons),
             ("car", car),
             ("cdr", cdr)]

unaryOperator :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOperator f [v] = f v

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args  = if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do left <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return . show $ s
unpackStr (Bool s) = return . show $ s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackChar :: LispVal -> ThrowsError Char
unpackChar (Char c) = return c
unpackChar notChar = throwError $ TypeMismatch "char" notChar


unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

-- Other functions
eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool (arg1 == arg2)
eqv [Number arg1, Number arg2] = return $ Bool (arg1 == arg2)
eqv [String arg1, String arg2] = return $ Bool (arg1 == arg2)
eqv [Atom arg1, Atom arg2] = return $ Bool (arg1 == arg2)
eqv [List arg1, List arg2] = return $ Bool ((length arg1 == length arg2) && (and $ zipWith (==) arg1 arg2)) --different from Tang's
eqv [_, _] = return $ Bool False
eqv badArgsList = throwError $ NumArgs 2 badArgsList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackerEqual :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackerEqual arg1 arg2 (AnyUnpacker unpacker) =
  do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return (unpacked1 == unpacked2)
    `catchError` (const (return $ False))

listEquals :: LispVal -> LispVal -> ThrowsError LispVal
listEquals (List list1) (List list2) =
  let
    Right boolList = sequence $ zipWith (\x y -> equal [x, y]) list1 list2
  in
    (liftM Bool $ liftM ((length list1 == length list2) &&) $ liftM or $ mapM unpackBool boolList)
    `catchError` (const $ return $ Bool False)

listEquals _ _ = return $ Bool False

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] =
  do
    primitiveEquals <- liftM or $ mapM (unpackerEqual arg1 arg2) [AnyUnpacker unpackNum,
                                                                  AnyUnpacker unpackStr,
                                                                  AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    lstEquals <- listEquals arg1 arg2
    return $ Bool (primitiveEquals || let Bool x = eqvEquals in x || let Bool x = lstEquals in x)

equal badArgList = throwError $ NumArgs 2 badArgList

-- Implement basic Lisp handling
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return (List xs)
cdr [DottedList (x:[]) y ] = return (List [y])
cdr [DottedList (x:xs) y ] = return (DottedList xs y)
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return (List [x])
cons [x, List y] = return (List ([x] ++ y))
cons [x, DottedList ys y] = return (DottedList (x:ys) y)
cons [x, y] = return (DottedList [x] y)
cons [badArg] = throwError $ TypeMismatch "pair" badArg
cons badArgList = throwError $ NumArgs 1 badArgList

cond :: [LispVal] -> ThrowsError LispVal
-- I have left this here so I can comeback and refactor the cond operator
-- For now it relies on eval 'else to return true. I am not sure if that's the best
-- way to go about this.
-- cond args = case (head . last $ list) of
                  -- (Atom "else") -> if (extractValue . cond (init args)) == Bool True
                  --                                then (cond (init args))
                  --                                    else last . last $ list
                  -- _ -> last . head $ filteredList

cond args =
  last . head $ filteredList
     where
       unlist (List x) = x
       list = map unlist args
       -- Perhaps, I shouldn't evaluate the whole list here?
       evaluatedList = (eval <$>) <$> list -- map (map eval) list
       filteredList = filter ((Bool True ==) . extractValue . head) evaluatedList

evaluator :: String -> LispVal
evaluator = extractValue . join . liftM eval . readExpr

main :: IO ()
main =
  do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled

testList =  TestList $ map TestCase
    [assertEqual "" (Number 2) (evaluator "(+ 1 1)"),
     assertEqual "" (Number 3) (evaluator "(- (+ 4 6 3) 3 5 2)"),
     assertEqual "" (Bool True) (evaluator "(number? 123)"),
     assertEqual "" (Bool True) (evaluator "(string? \"hello\")"),
     assertEqual "symbol type-testing" (Bool True) (evaluator "(symbol? 'hello)"),
     assertEqual "list type-testing" (Bool True) (evaluator "(list? '(1 2 3))"),
     assertEqual "symbol to string" (extractValue $ readExpr "flying-fish") (evaluator "(symbol->string 'flying-fish)"),
     assertEqual "implement if statement" (String "yes") (evaluator "(if (> 2 3) \"no\" \"yes\")"),
     assertEqual "implement if statement" (Number 9) (evaluator "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")"),

-- implement cond expressions
     assertEqual "implement cond" (evaluator "'greater") (evaluator "(cond ((> 3 2) 'greater) ((< 3 2) 'less))"),
     assertEqual "implement cond" (evaluator "'equal") (evaluator "(cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))"),

     assertEqual "implement case" (evaluator "'composite") (evaluator "(case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite))"),
     assertEqual "implement case" (evaluator "'consonant") (evaluator "(case (car '(c d)) ((a e i o u) 'vowel) ((w y) 'semivowel) (else 'consonant))"),
     assertEqual "implement case" (evaluator "'semivowel") (evaluator "(case (car '(w d)) ((a e i o u) 'vowel) ((w y) 'semivowel) (else 'consonant))"),

-- string functions
     assertEqual "string?" (Bool True) (evaluator "(string? \"Hello\")"),
     assertEqual "make-string" (String "  ") (evaluator "(make-string 2)"),
     assertEqual "make-string" (String "aa") (evaluator "(make-string 2 #\\a)"),
     assertEqual "string" (String "Apple") (evaluator "(string #\\A #\\p #\\p #\\l #\\e)"),
     assertEqual "string-length" (Number 5) (evaluator "(string-length \"Apple\")"),
     assertEqual "string-ref" (Char 'l') (evaluator "(string-ref \"Apple\" 3)"),
     assertEqual "substring" (String "pp") (evaluator "(substring \"Apple\" 1 3)"),
     assertEqual "string-append" (String "AppleBanana") (evaluator "(string-append \"Apple\" \"Banana\")"),
     assertEqual "string->list" (List [Char 'A', Char 'b', Char 'c']) (evaluator "(string->list \"Abc\")"),
     assertEqual "list->string" (String "Cmsk") (evaluator "(list->string '(#\\C #\\m #\\s #\\k))"),
     -- Implement string->immutable-string ?
     -- string-set!, string-copy, string-copy!, string-fill!, string-append, string->list, list->string, build-string

-- test list primitive functionality
     assertEqual "implement eq?" (Bool True) (evaluator "(eq? 'a 'a)"),
     assertEqual "implement eq?" (Bool False) (evaluator "(eq? 'b 'a)"),
     assertEqual "implement eq?" (Bool True) (evaluator "(eq? '() '())"),
     assertEqual "implement eq?" (Bool False) (evaluator "(eq? '(1 2 3) '(1 2 3 4))"),

     assertEqual "implement equal" (Bool False) (evaluator "(equal? '(1 2 3) '(1 2 3 4))"),
     assertEqual "implement equal" (Bool False) (evaluator "(equal? 'b 'a)"),
     assertEqual "implement equal" (Bool True) (evaluator "(equal? 'a 'a)"),
     assertEqual "implement equal" (Bool True) (evaluator "(equal? '() '())"),
     assertEqual "implement equal" (Bool True) (evaluator "(equal? '(1 2) '(1 2))"),
     assertEqual "implement equal" (Bool True) (evaluator "(equal? 2 \"2\")"),
     assertEqual "implement equal" (Bool True) (evaluator "(equal? '(1 \"2\") '(1 2))"),

     assertEqual "implement cons" (extractValue $ readExpr "(1 . 2)") (evaluator "(cons 1 2)"),
     assertEqual "implement cons" (extractValue $ readExpr "(1 2 3)") (evaluator "(cons 1 '(2 3))"),

     assertEqual "implement car" (Number 1) (evaluator "(car '(1 2))"),
     assertEqual "implement cdr" (List [Number 2, Number 3]) (evaluator "(cdr '(1 2 3))"),
     assertEqual "implement cdr" (extractValue $ readExpr "()") (evaluator "(cdr '(1))"),
     assertEqual "implement cdr" (extractValue $ readExpr "(2)") (evaluator "(cdr '(1 . 2))"),
     assertEqual "implement cdr" (extractValue $ readExpr "(2)") (evaluator "(cdr '(1 . 2))"),
     assertEqual "implement cdr" (extractValue $ readExpr "(2 . 3)") (evaluator "(cdr '(1 2 . 3))")]

tests = runTestTT testList

{-list = List [Atom "cond",List [List [Atom ">",Number 3,Number 2],List [Atom "quote",Atom "greater"]],List [List [Atom "<",Number 3,Number 2],List [Atom "quote",Atom "less"]]]-}
