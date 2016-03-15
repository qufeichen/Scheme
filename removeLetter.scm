; (removeLetter str letter)
; function that removes all occurances of a letter from a string

(define (removeLetter str l)
  (define (calc s l)
    (if (null? s)
         '()
         (if (char=? (car s) l)
             (calc (cdr s) l)
             (append (list (car s)) (calc (cdr s) l))
         )
     )
   )
  
  ( list->string (calc (string->list str) l))
   
)