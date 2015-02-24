;calculate pascals triangle in scheme 
; the elements at the bottom are the sum of elements at the top
;                   1                  - triangle 0
;                 1   1                 - triangle 1
;                1   2  1                - triangle 2
;               1  3   3  1               - triangle 3
;              1  4  6   4   1              - triangle 4
;             1 5 10 10 5 1
;build a list of triangles ((1) (1 1))- triangle 1  (1) -triangle(0)

;main method - it will build a tree for the triangle elements
(define (build-triangle n)
  (define i 0)
  (define (build-triangle-iter n i)
    ;(display i)
    (if (= n i)  (list (build-next-times i))
        (cons (build-next-times i)  (build-triangle-iter n (+ i 1)))
        ))
  (build-triangle-iter n 0)
  )

;build the next iteration a number of t times 
(define (build-next-times t)
  (if (= t 0)  (build-next `())
      (build-next (build-next-times (- t 1)))
      )
  )
;based on the current iteration build the next one
(define (build-next current-list)
  (if (null? current-list) '(1)
      (let ((new-list `()))        
        (cons 1 (reverse (cons 1 (reduced-sum current-list ))) ))
      )
  )
(define (generate-index list)
  (define (generate-index-reverse list)
    (if (null? list) '() 
        (cons (length (cdr list)) (generate-index-reverse (cdr list) ))
        ))
  (reverse (generate-index-reverse list))
  )

;add the sum of the top neighbours
(define (reduced-sum current-list)
  (define new-list `())
  (map 
   (lambda(x) (car (cons  (apply +  (list (list-ref current-list  (+ x  1) ) (list-ref current-list x))) new-list)))
   (generate-index (cdr current-list))))