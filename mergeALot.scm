; (mergeALot l)
; program that takes a list of sorted lists and merges them into one sorted list.
; If the top-level list contains entries which are not sorted, then an error message is returned indicating which entry is unsorted.
;
; i.e. 
; (mergeALot '((1 3 5) (2 4 6) (2 6 8)))
; => (1 2 2 3 4 5 6 6 8)
;
; (mergeALot '((1 3 5) (2 0 6) (2 6 8) (3 1) (5 4 3 2 1 0) (1 5 9 13 17)))
; => "ERROR: The following lists are not sorted: (2 4 5)
;
; (mergeALot '((1 4 7) (5 6 1)))
; => "ERROR: The following lists are not sorted: (2)

#lang racket

(define (mergeALot l)
  ;function to check if list of lists is sorted
  ;returns a list of unsorted lists
  (define (sortlist l indx)
    ( if (null? l)
         '()
         (if (equal? (sort (car l) <) (car l))
             (sortlist (cdr l) (+ indx 1))
             (append (list indx) (sortlist (cdr l) (+ indx 1)))
         )
     )
    )
  ;function that checks if list is sorted
  (define (isSorted x)
    (if (equal? (sort x <) x)
        1
        0
       )
   )
  ;function to merge lists
  (define (merge l)
    (if (null? l)
        '()
        (append (car l) (merge (cdr l)))
        )
    )

  (if (null? (sortlist l 1))
      (sort (merge l) <)
      (begin (display "ERROR: the following lists are not sorted: ") (display (sortlist l 1)))
  )
)
