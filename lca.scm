; (lca tree k1 k2)
; program that finds the lowest common ancestor of nodes k1 and k2 in tree t
; program prints the subtree t such that the root of t is the lca of k1 and k2
;
; (lca '(73 (31 (5 () ()) ()) (101 (83 () (97 () ())) (2016 () ()))) 97 2016)   
; => (101 (83 () (97 () ())) (2016 () ()))

(define (lca t k1 k2)
  (cond ((null? t) '() )
        ((and (< k1 (car t)) (< k2 (car t))) (lca (car (cdr t)) k1 k2))
        ((and (> k1 (car t)) (> k2 (car t))) (lca (car (cddr t)) k1 k2))
        (#t t)
      )
  )
