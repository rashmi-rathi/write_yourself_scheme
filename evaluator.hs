import System.Environment
import Parser
import Test.HUnit

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
