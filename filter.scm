; (filter f l)
; function that applies a filter operation on a list (takes the weighted average of neighbouring elements in a list).
; resulting list is obtained by first normalizing the filer coefficients (f) to sum to 1.
; the normalized filter coefficients are then applied to the list.
; 
; i.e 
; (filter '(1 2 3 4) '(1.0 1.0 2.0 3.0 5.0 8.0 13.0))
; => '(2.1 3.4 5.5 8.9 6.0 3.4 1.3)
; 
;; filter = '(1 2 3 4)
;; normalized filter = '(1/10 2/10 3/10 4/10)
;; filters applied to list = '(1/10*1.0 + 2/10*1.0 + 3/10*2.0 + 4/10*3.0) = 2.1
;;                           '(1/10*1.0 + 2/10*2.0 + 3/10*3.0 + 4/10*5.0) = 3.4
;;                           '(1/10*2.0 + 2/10*3.0 + 3/10*5.0 + 4/10*8.0) = 5.5
;;                           '(1/10*3.0 + 2/10*5.0 + 3/10*8.0 + 4/10*13.0) = 8.9
;;                           '(1/10*5.0 + 2/10*8.0 + 3/10*13.0) = 6.0
;;                           '(1/10*8.0 + 2/10*13.0) = 3.4
;;                           '(1/10*13.0) = 1.3 

(define (filter f l)
  ; function to sum up elements in filter
  (define (sum-list l)
    (if (null? l)
      0
      (+ (car l) (sum-list (cdr l)))
    )
  )
  ; normalize filter weights
  (define (normalize f s)
    ( if (null? f)
      '()
      (append (list(/ (car f) s)) (normalize (cdr f) s))
    )
  )
  ; multiply in filter elements
  ( define (multiply new_f l)
     (if (or (null? l) (null? new_f))
      0
      (+ (* (car l) (car new_f)) (multiply (cdr l) (cdr new_f)))
     )
  )
   ; Apply filter to list
  ( define (comp new_f l)
     (if (null? l)
      '()
      (append (list (multiply new_f l)) (comp new_f (cdr l)))
      )
  )
  
  ; call working method
    (comp (normalize f (sum-list f)) l)
  )