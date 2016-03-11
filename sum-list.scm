; (sum-list l)
; (rsum-list l 0)
; functions that determines the sum of the elements in a list.

; recursive method
(define (sum-list L)
  (if (null? L)
      0
      (+ (car L) (sum-list (cdr L)))
  )
)

; tail recursive method (using an accumulator)
(define (rsum-list L acc)
  (if (null? L)
      acc
      (rsum-list (cdr L) (+ acc (car L)))
      )
  )
