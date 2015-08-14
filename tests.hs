import Data.Char
import Test.HUnit
import Parser

testCase1 = (assertEqual "" (Char ' ') (readExpr "#\\space"))
testCase2 = assertEqual "" (String "YOLO\"OLOY") (readExpr "\"YOLO\\\"OLOY\"")
testCase3 = assertEqual "" (String "Dil\nLid") (readExpr "\"Dil\nLid\"" )
testCaseOct = assertEqual "" (Number 83) (readExpr "#o123")
testCaseHex = assertEqual "" (Number 291) (readExpr "#x0123")

main = runTestTT . TestList $ map TestCase [testCase1, testCase2, testCase3, testCaseOct, testCaseHex]
