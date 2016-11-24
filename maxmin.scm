; (maxmin l)
; simple scheme program that takes a list as input, and returns the a list with the largest and smallest numbers in the input list

#lang racket

(define (maxmin l)

  (define (getmax l minmax)
    (if (null? l)
        minmax
          
        (cond
          ((< (car l) (car minmax) ) (getmax (cdr l) (cons (car l) (cdr minmax))) )
          ((> (car l) (cdr minmax) ) (getmax (cdr l) (cons (car minmax) (car l))) )
          (else (getmax (cdr l) minmax))
          )        
    )
  )

  (getmax l (cons (car l) (car l)))
    
   )