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
    [(list (complex-num x1 y1) (complex-num x2 y2))
     (complex-num (+ x1 x2) (+ y1 y2))]))

(define (complex-mul x y)
  (match (list x y)
    [(list (complex-num x1 y1) (complex-num x2 y2))
     (complex-num (- (* x1 x2) (* y1 y2)) (+ (* x1 y2) (* y1 x2)))]))

(define (complex-div x y)
  (match (list x y)
    [(list (complex-num x1 y1) (complex-num x2 y2))
     (complex-num (quotient x1 x2) (quotient y1 y2))]))

(define (part-1 a)
  (foldl (lambda (_x acc)
           (let* ([first (complex-mul acc acc)]
                  [second (complex-div first (complex-num 10 10))])
             (complex-add second a)))
         (complex-num 0 0)
         (list 1 2 3)))

(define (display-complex-num x)
  (match x
    [(complex-num x y)
     (string-append "[" (number->string x) "," (number->string y) "]")]))

(module+ test
  (require rackunit)

  (define a (complex-num 1 1))
  (define b (complex-num 2 2))
  (define c (complex-num 2 5))
  (define d (complex-num 3 7))
  (define e (complex-num -2 5))
  (define f (complex-num 10 -1))
  (define g (complex-num -1 -2))
  (define h (complex-num -3 -4))
  (define i (complex-num 10 12))
  (define j (complex-num 11 12))
  (define k (complex-num 3 5))
  (define l (complex-num -10 -12))
  (define m (complex-num -11 -12))

  (check-equal? (complex-add a b) (complex-num 3 3) "complex-add example")
  (check-equal? (complex-add c d) (complex-num 5 12) "complex-add example")
  (check-equal? (complex-add e f) (complex-num 8 4) "complex-add example")
  (check-equal? (complex-add g h) (complex-num -4 -6) "complex-add example")

  (check-equal? (complex-mul a b) (complex-num 0 4) "complex-mul example")
  (check-equal? (complex-mul c d) (complex-num -29 29) "complex-mul example")
  (check-equal? (complex-mul e f) (complex-num -15 52) "complex-mul example")
  (check-equal? (complex-mul g h) (complex-num -5 10) "complex-mul example")

  (check-equal? (complex-div i b) (complex-num 5 6) "complex-div example")
  (check-equal? (complex-div j k) (complex-num 3 2) "complex-div example")
  (check-equal? (complex-div l b) (complex-num -5 -6) "complex-div example")
  (check-equal? (complex-div m k) (complex-num -3 -2) "complex-div example")

  (check-equal? (part-1 (complex-num 25 9))
                (complex-num 357 862)
                "part 1 example")
  (check-equal? (part-1 (complex-num 166 51))
                (complex-num 404104 928207)
                "part 1 test"))
