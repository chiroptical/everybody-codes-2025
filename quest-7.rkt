#lang racket

(provide notes
         rule
         parse-rule
         part-1
         is-success?)

(require "util.rkt")

(struct notes (names rules) #:transparent)

(struct rule (first in) #:transparent)

(define (parse-rule l)
  (match (string-split l ">")
    [(list first rest) (rule (string-trim first) (csv (string-trim rest)))]))

(define (read-notes file)
  (call-with-input-file file
                        (lambda (in)
                          (let ([names-in (read-line in)]
                                [_ (read-line in)]
                                [rules (in-lines in)])
                            (notes (csv names-in)
                                   (map parse-rule (sequence->list rules)))))))

(define (list-ref-default lst index default)
  (with-handlers ([exn:fail? (lambda (_) default)])
    (list-ref lst index)))

(define (get-indexes hay nee)
  (indexes-of (string->list hay) (first (string->list nee))))

(define (is-success? l r)
  (match r
    [(rule fst rest)
     (ormap (lambda (good-char)
              (let* ([indexes (get-indexes l fst)]
                     [c (first (string->list good-char))]
                     ; if any of those chars are in x
                     )
                (if (empty? indexes)
                    #t
                    (ormap (lambda (x)
                             (let ([post (list-ref-default (string->list l)
                                                           (+ x 1)
                                                           #f)])
                               (match post
                                 [#f #f]
                                 [_ (char=? post c)])))
                           indexes))))
            rest)]))

(define (part-1 file)
  (let* ([my-notes (read-notes file)]
         [rules (notes-rules my-notes)])
    (filter (lambda (name) (andmap (lambda (r) (is-success? name r)) rules))
            (notes-names my-notes))))
