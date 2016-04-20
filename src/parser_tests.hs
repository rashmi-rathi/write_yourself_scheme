import Data.Char
import Data.Complex
import Data.Ratio
import Test.HUnit
import Parser

main = runTestTT . TestList $ map TestCase [assertEqual "" (Char ' ') (readExpr "#\\space"),
                                            assertEqual "" (Char 'A') (readExpr "#\\A"),
                                            assertEqual "" (String "YOLO\"OLOY") (readExpr "\"YOLO\\\"OLOY\""),
                                            assertEqual "" (String "Dil\nLid") (readExpr "\"Dil\nLid\"" ),
                                            assertEqual "" (Number 83) (readExpr "#o123"),
                                            assertEqual "" (Number 291) (readExpr "#x0123"),
                                            assertEqual "" (Float 291.99) (readExpr "#d291.99"),
                                            assertEqual "" (Complex (1 :+ 1)) (readExpr "1+1i"),
                                            assertEqual "" (Rational (5 % 7)) (readExpr "5/7"),
                                            assertEqual "" (List [Number 1, Number 2, Number 3]) (readExpr "(1 2 3)"),
                                            assertEqual "" (List [Atom "quasiquote",List [Number 1,Number 2,List [Atom "unquote",List [Atom "+",Number 3,Number 4]]]]) (readExpr "`(1 2 ,(+ 3 4))")
                                            ]
