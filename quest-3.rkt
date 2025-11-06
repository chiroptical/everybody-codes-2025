#lang racket

(provide part-1)

(require racket/set)
(require "util.rkt")
(require data/collection)

(define (part-1 file)
  (let* ([crates (call-with-input-file file
                                       (lambda (test-in)
                                         (let ([crates-txt (read-line test-in)])
                                           (csv crates-txt))))]
         [crates-set (list->set crates)])
    (foldl (lambda (result x) (+ result (string->number x))) 0 crates-set)))
