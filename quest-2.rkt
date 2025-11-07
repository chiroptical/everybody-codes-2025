#lang racket

(provide complex-num
         complex-add
         complex-mul
         complex-div
         part-1)

; We can't use built in rationals because the sign matters,
; e.g. (numerator (10 / -1)) yields -10 but our addition
; operator needs it to return 10.
(struct complex-num (x y) #:transparent)

(define (complex-add x y)
  (match (list x y)
    [(list (complex-num x1 y1) (complex-num x2 y2)) (complex-num (+ x1 x2) (+ y1 y2))]))

(define (complex-mul x y)
  (match (list x y)
    [(list (complex-num x1 y1) (complex-num x2 y2))
     (complex-num (- (* x1 x2) (* y1 y2)) (+ (* x1 y2) (* y1 x2)))]))

(define (complex-div x y)
  (match (list x y)
    [(list (complex-num x1 y1) (complex-num x2 y2)) (complex-num (quotient x1 x2) (quotient y1 y2))]))

(define (part-1 a)
  (foldl (lambda (_x acc)
           (let* ([first (complex-mul acc acc)]
                  [second (complex-div first (complex-num 10 10))])
             (complex-add second a)))
         (complex-num 0 0)
         (list 1 2 3)))

(define (display-complex-num x)
  (match x
    [(complex-num x y) (string-append "[" (number->string x) "," (number->string y) "]")]))

(display-complex-num (part-1 (complex-num 25 9)))
(display-complex-num (part-1 (complex-num 166 51)))
