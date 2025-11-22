#lang racket

(provide csv
         todo
         point
         render-mat)

(define (csv in)
  (string-split in ","))

(define (todo)
  (error "not implemented yet"))

(struct point (r c))

(define (render-mat mat dims)
  (match dims
    [(point rows cols)
     (for* ([r rows]
            [c cols])
       (let ([display-char (if (set-member? mat (point r c)) #\X #\.)])
         (display display-char)
         (if (= c (- cols 1))
             (display "\n")
             #f)))]))
