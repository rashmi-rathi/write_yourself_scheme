import Test.HUnit

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
     -- string-set!, string-copy, string-copy!, string-fill!, build-string

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


