; multi-part program that manipulates cubes/cube roots

; (cubeLess x b)
; calculates the power of 3 and the remainder such that b=(x^3)+r
; i.e.
; (cubeLess 2 10)
; => 2

(define (cubeless x b)
  (- b (expt x 3))
)


; (smallerCube b)
; finds all numbers with a cube smaller that the given limit, and generates the results as a list
; i.e.
; (smallerCube 130)
; => ((1 129) (2 122)(3 103)(4 66)(5 5))


(define (smallerCube b)
  (define (calculate b n)
    (if (< (cubeless n b) 0)
        '()
        (append (list (append (list n) (list (cubeless n b)))) (calculate b (+ n 1)))
    )
  )

  (calculate b 1)
)


; (restSum s)
; adds up all the remainders from finding all the cubes up to an upper limit
; i.e.
; (restSum (smallerCube 130))
; => 425

(define (restSum s)
  (if (null? s)
      0
      (+ (car (cdr (car s))) (restSum (cdr s)))
  )
)


; (showAllRestSums min max)
; print all rest sums that are multiples of 3 within a range
; i.e.
; (showAllRestSum 1 20)
; => '((1 0) (4 3) (7 6) (9 9) (12 15) (15 21) (18 27))

(define (showAllRestSum minNum maxNum)
  (define (isMult3 n)
    (if (= (modulo (restSum (smallerCube n)) 3) 0)
        (list (list n (restSum(smallerCube n))))
        '()
    )
  )

  (if(> minNum maxNum)
     '()
     (append (isMult3 minNum) (showAllRestSum (+ minNum 1) maxNum))
  )
)
