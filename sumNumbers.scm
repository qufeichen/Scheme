; (sumNumbers l)
; function that takes a list of characters and numbers, and returns the sum of the numbers and a list of all the characters (unchanged).
;
; i.e. (sumNumbers '(a 2 4 a b 5))
; => '((a a b) 11)

(define (sumNumbers l)
  (define (getSum l)
    (if (null? l)
      0
      (if (number? (car l))
          (+ (car l) (getSum (cdr l)))
          (getSum (cdr l))
      )
    )
  )
  (define (getList l)
    (if (null? l)
      '()
      (if (integer? (car l))
          (getList (cdr l))
          (append (list (car l)) (getList (cdr l)))
      )
    )
  )
  
  (append (list (getList l)) (list (getSum l)))    
)