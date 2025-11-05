#lang racket

(provide complex-num
         complex-add
         complex-mul
         complex-div)

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
