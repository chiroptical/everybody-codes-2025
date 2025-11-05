#lang racket

(require rackunit
         "quest-2.rkt")

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
              "part 1 test")
