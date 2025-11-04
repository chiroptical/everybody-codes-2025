#lang racket

(require data/collection)

(define (part-1 file)
  ; TODO: Can I avoid having to remember to `close-input-port`?
  (define test-in (open-input-file file))

  (define names-in (read-line test-in))
  (define _ (read-line test-in))
  (define steps-in (read-line test-in))

  (close-input-port test-in)

  (define names (string-split names-in ","))
  (define steps (string-split steps-in ","))

  (define zero (char->integer #\0))

  (define (read-char x)
    (- (char->integer x) zero))

  (define (add- top x y)
    (min (+ x y) top))

  (define (sub- bot x y)
    (max (- x y) bot))

  (define clamp (- (length names) 1))

  (define idx
    ; data/collection has accumulator first in lambda
    ; base:foldl has it last
    (foldl (lambda (result x)
             (match (string->list x)
               [(list #\L amt) (sub- 0 result (read-char amt))]
               [(list #\R amt) (add- clamp result (read-char amt))]))
           0
           steps))
  (nth names idx))

(define (part-2 _file)
  "TODO")

(provide part-1
         part-2)
