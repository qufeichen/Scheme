; (range min max)
; function that creates a list with integers in a given range.

(define (range x y)
  (if (= x y)
    (list y)
    (append (list x) (range (+ x 1) y))
  )
)
