; (reverse l)
; simple scheme program that reverses the elements of a list

#lang racket


(define (reverse l)

  (define (reverselist l returnlist)
    (if (null? l)
      returnlist
      (reverselist (cdr l) (append (list (car l)) returnlist))
    )
  )

  (reverselist l '())
  )
  