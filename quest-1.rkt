#lang racket

(provide part-1
         part-2)

(require "util.rkt")

(struct notes (names steps))

(define (read-notes file)
  (call-with-input-file file
                        (lambda (test-in)
                          (let ([names-in (read-line test-in)]
                                [_ (read-line test-in)]
                                [steps-in (read-line test-in)])
                            (notes (csv names-in) (csv steps-in))))))

(define (char-to-integer x)
  (- (char->integer x) (char->integer #\0)))

(define (clamp-add top x y)
  (min (+ x y) top))

(define (clamp-sub bot x y)
  (max (- x y) bot))

(define (part-1 file)
  (let* ([my-notes (read-notes file)]
         [clamp-max (- (length (notes-names my-notes)) 1)]
         [idx (foldl
               (lambda (x result)
                 (match (string->list x)
                   [(list #\L amt) (clamp-sub 0 result (char-to-integer amt))]
                   [(list #\R amt)
                    (clamp-add clamp-max result (char-to-integer amt))]))
               0
               (notes-steps my-notes))])
    (list-ref (notes-names my-notes) idx)))

(define (part-2 _file)
  "TODO")
