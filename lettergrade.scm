;(lettergrade X)
; simple program that takes in a percentage, and computes the UOttawa equivalent letter grade

(define (lettergrade x)
  ( cond
     ((>= x 90) '(A+))
     ((>= x 85) '(A))
     ((>= x 80) '(A-))
     ((>= x 75) '(B+))
     ((>= x 70) '(B))
     ((>= x 65) '(C+))
     ((>= x 60) '(C))
     ((>= x 55) '(D+))
     ((>= x 50) '(D))
     ((>= x 40) '(E))
     (#t '(F))
     ))
