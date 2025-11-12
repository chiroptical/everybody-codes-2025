#lang racket

(provide part-1)

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
