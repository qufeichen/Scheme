; (ourLog x e)
; function that accepts a number -1 < x < 1 and a precision 0 < e < 1, and returns the value of log(1+x) with a precison e, and the number of terms necessary to reach this precision
; precision is defined as the difference between the results with n terms and with n-1 terms
; precision = e = abs[ (product with n-1) - (product with n) ]
;
; i.e.
; (ourLog -0.5 0.001)
; => '(-0.6927501860119046 9)
;
; the precision at 9 terms is e = (−0.6927501860119046 − (−0.6922619047619046)) = 0.0004882812 
; this is the first time that the precision is equal or better than the specified e = 0.001

#lang racket

(define (ourLog x e)
  ;calculate log(1+x)
   (define (LogOnePlusX x count)
    (if (eq? count 0)
        0
        (if (eq? (modulo count 2) 0)
            (+ (- 0 (/ (expt x count) count)) (LogOnePlusX x (- count 1)))
            (+ (/ (expt x count) count) (LogOnePlusX x (- count 1)))
            )
        )
    )
  ;calculates precision for p terms
  (define (getPrecision x p)
    (if(eq? p 0)
       0
       (abs (- (LogOnePlusX x (- p 1)) (LogOnePlusX x p)))
       )
    )
  ;calculate result
  (define (getResult x e i)
    (if(<= (getPrecision x i) e)
       (list (LogOnePlusX x i) (+ i 1))
       (getResult x e (+ i 1))
     )
    )

  (getResult x e 1)
)
