(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (cdr lat))
     (else  (cons (car lat)
                  (rember a
                          (cdr lat)))))))

(equal?
 (rember 'mint '(lamb chops and mint jelly))
 '(lamb chops and jelly))

(equal?
 (rember 'toast '(bacon lettuce and tomato))
 '(bacon lettuce and tomato))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else
      (cons (car (car l)) (firsts (cdr l)))))))

(firsts '((apple peach pumpkin) (plum pear cherry) (grape raisin pea) (bean carrot eggplant)))


(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? old (car lat)) (cons (car lat) (cons new (cdr lat))))
            (else (cons (car lat) (insertR new old (cdr lat)))))))))

(equal? (insertR 'topping 'fudge '(ice cream with fudge for dessert))
        '(ice cream with fudge topping for dessert))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? old (car lat)) (cons new lat))
            (else (cons (car lat) (insertL new old (cdr lat)))))))))

(equal? (insertL 'topping 'fudge '(ice cream with fudge for dessert))
        '(ice cream with topping fudge for dessert))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new (cdr lat)))
     (else (cons (car lat) (subst new old (cdr lat)))))))

(equal? (subst 'fudge 'topping '(ice cream with topping for dessert))
        '(ice cream with fudge for dessert))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
     (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(equal? (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
        '(vanilla ice cream with chocolate topping))

(define multiremeber
  (lambda (a lat)
    (cond ((null? lat) '())
          (else (cond
                 ((eq? a (car lat)) (multiremeber a (cdr lat)))
                 (else (cons (car lat) (multiremeber a (cdr lat)))))))))

(equal? (multiremeber 'cup '(coffee cup tea cup and hick cup))
        '(coffee tea and hick))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
     (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(equal? (multiinsertR 'x 'd '(a b c d e d d f))
        '(a b c d x e d x d x f))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
     (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(equal? (multiinsertL 'x 'd '(a b c d e d d f))
        '(a b c x d e x d x d f))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat)
      '())
     ((eq? old (car lat))
      (cons new (multisubst new old (cdr lat))))
     (else
      (cons (car lat) (multisubst new old (cdr lat)))))))

(equal? (multisubst 'x 'a '(a b c d e a a b))
        '(x b c d e x x b))

