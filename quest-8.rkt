#lang racket

(provide part-1
         part-2
         overlaps?
         vec)

(require "util.rkt")

(define (read-notes file)
  (call-with-input-file file
                        (lambda (in)
                          (let* ([names-in (read-line in)]
                                 [as-csv (csv names-in)])
                            (map string->number as-csv)))))

(require racket/list/grouping)

(define (part-1 file n)
  (let ([notes (read-notes file)]
        [midpoint (/ n 2)])
    (foldl (lambda (window acc)
             (match window
               [(list x y)
                (if (= midpoint (abs (- x y)))
                    (+ 1 acc)
                    acc)]))
           0
           (windows 2 1 notes))))

(struct vec (lower upper) #:transparent)

(struct state (prev knots) #:transparent)

(define (overlaps? v1 v2)
  (let ([x (vec-lower v1)]
        [y (vec-upper v1)]
        [a (vec-lower v2)]
        [b (vec-upper v2)])
    (or (and (> a x) (< a y) (> b y)) (and (> x a) (< x b) (> y b)))))

(define (next-state vec s)
  (let* ([prev (state-prev s)]
         [knots (state-knots s)]
         [new-knots (foldl (lambda (p-vec acc)
                             (if (overlaps? p-vec vec)
                                 (+ acc 1)
                                 acc))
                           knots
                           prev)])
    (state (append prev (list vec)) new-knots)))

(define (make-vecs x)
  (match x
    [(list y z)
     (if (> y z)
         (vec z y)
         (vec y z))]))

(define (part-2 file)
  (let* ([notes (read-notes file)]
         [vecs (map make-vecs (windows 2 1 notes))]
         [new-state (foldl next-state (state (list) 0) vecs)])
    (state-knots new-state)))
