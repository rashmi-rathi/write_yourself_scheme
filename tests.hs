import Data.Char
import Data.Complex
import Data.Ratio
import Test.HUnit
import Parser

testCase1 = (assertEqual "" (Char ' ') (readExpr "#\\space"))
testCase2 = assertEqual "" (String "YOLO\"OLOY") (readExpr "\"YOLO\\\"OLOY\"")
testCase3 = assertEqual "" (String "Dil\nLid") (readExpr "\"Dil\nLid\"" )
testCaseOct = assertEqual "" (Number 83) (readExpr "#o123")
testCaseHex = assertEqual "" (Number 291) (readExpr "#x0123")
testCaseDec = assertEqual "" (Float 291.99) (readExpr "#d291.99")
testCaseComplex = assertEqual "" (Complex (1 :+ 1)) (readExpr "1+1i")
testCaseRational = assertEqual "" (Rational (5 % 7)) (readExpr "5/7")

main = runTestTT . TestList $ map TestCase [testCase1, testCase2, testCase3, testCaseOct, testCaseHex, testCaseDec, testCaseComplex, testCaseRational]
