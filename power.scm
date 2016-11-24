; (power A B)
; simple scheme function that takes in two integers A and B, and returns A raised to the B power
; (A^B)

#lang racket

(define (power A B)
  (if (= B 1)
      A
      (* A (power A (- B 1)))
      )
  )
