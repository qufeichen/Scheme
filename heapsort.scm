; Heapsort algorithm 
; takes in a simple list l as input, and returns a sorted list of all atoms in l by applying the heapsort algorithm
;
; (heapsort l)

#lang racket

(define (heapsort l)

  ; method to calculate parent index of node i
  (define (parentIndex i)
    (floor (/ (- i 1) 2))
    )

  ; method to calculate the left child index of node i
  (define (leftChildIndex i)
    (+ (* 2 i) 1)
    )
  ; method to calculate right child index of node i
  (define (rightChildIndex i)
    (+ (* 2 i) 2)
    )

  ; returns the element at a given index in list l
  (define (getElemAtPosition l index)
    (if (= index 0)
        (car l)
        (getElemAtPosition (cdr l) (- index 1))
        )
    )

  ; swap method takes in the list and two indexes
  ; method will return a list with the elements at the two indexes swapped
   (define (swap l x y)
    (define (get-elem l index)
      (if (= index 0)
          (car l)
          (get-elem (cdr l) (- index 1))
          )
      )
    ; replaces the element at the current index with the new-element (element from the new-index)
     (define (replace-elem l current-index new-elem)
        (if (= current-index 0)
          (cons new-elem (cdr l))
          (cons (car l) (replace-elem (cdr l) (- current-index 1) new-elem))
          )
      )

    ; get the values of the elements of both indexes
    ; call replace-elem twice, using the resulting list from the first call as the input to the second call
    (let ([elem-x (get-elem l x)] [elem-y (get-elem l y)])
      (replace-elem (replace-elem l y elem-x) x elem-y)
      )
    
    )


  ; siftdown method
  ; repairs a "broken heap"
  (define (siftDown l startIndex endIndex)
    
    (let ([rootIndex startIndex] [result-list l])
      (define (while-loop)
        (let ([child (leftChildIndex rootIndex)] [swapIndex rootIndex])
          (begin
            (cond [(< (getElemAtPosition result-list swapIndex)(getElemAtPosition result-list child))
                   (set! swapIndex child)]
                  [(and (<= (+ child 1) endIndex) (< (getElemAtPosition result-list swapIndex) (getElemAtPosition result-list (+ child 1))))
                   (set! swapIndex (+ child 1))])
            (if (= swapIndex rootIndex)
                       result-list
                       (begin
                         (set! result-list (swap result-list rootIndex swapIndex))
                         (set! rootIndex swapIndex)
                         (if (<= (leftChildIndex rootIndex) endIndex)
                             (while-loop)
                             result-list)))
            )
          )
        )
      ; call loop for the first time
       (if (<= (leftChildIndex rootIndex) endIndex)
           (while-loop)
           result-list)
      )
    )

  ; heapify
  ; creates an in-place heap of the provided "a" array
  (define (heapify l count)
    (let ([result l] [endIndex (- count 1)] [startIndex (parentIndex (- count 1))])
      (define (while_loop startIndex)
        (begin
          (set! result (siftDown result startIndex endIndex))
          (if (>= (- startIndex 1) 0)
              (while_loop (- startIndex 1))
              result)
        )
     )
    (if (>= startIndex 0)
        (while_loop startIndex)
        result
        )
      )
    )

  ; main execution starts here
  ; corresponds to "heapsort" procedure in the psuedo-code
  (let ([list l] [count (length l)])
    (begin
      (heapify l count)
      (let ([endIndex (- count 1)])
        (define (loop)
          (if (> endIndex 0)
                (begin
                  (set! list (swap list endIndex 0))
                  (set! endIndex (- endIndex 1))
                  (set! list (siftDown list 0 endIndex))
                  (loop))
              list)
          )
        (loop)
        )
      )
    )

  )