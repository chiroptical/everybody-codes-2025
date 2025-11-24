#lang racket

(provide notes
         rule
         parse-rule
         part-1
         is-success?
         search-for-failure)

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

(define (search-for-failure l indexes chars)
  (ormap
   (lambda (idx)
     (let ([follower-char (list-ref-default (string->list l) (+ idx 1) #f)])
       (match follower-char
         [#f #f]
         [_
          (andmap (lambda (c)
                    (not (char=? (first (string->list c)) follower-char)))
                  chars)])))
   indexes))

(define (is-success? l r)
  (match r
    [(rule fst rest)
     (let* ([indexes (get-indexes l fst)])
       (not (search-for-failure l indexes rest)))]))

(define (part-1 file)
  (let* ([my-notes (read-notes file)]
         [rules (notes-rules my-notes)])
    (filter (lambda (name) (andmap (lambda (r) (is-success? name r)) rules))
            (notes-names my-notes))))

(module+ test
  (require rackunit)

  (check-equal? (parse-rule "r > a,i,o")
                (rule "r" (list "a" "i" "o"))
                "parse rule works")

  (check-equal? (search-for-failure "Gaermirath" (list 1 7) (list "t" "l" "b"))
                #t
                "...")
  (check-equal? (search-for-failure "Oronris" (list 0) (list "r")) #f "...")
  (check-equal? (search-for-failure "Oronris" (list 0) (list "b")) #t "...")

  (check-equal? (is-success? "Oronris" (rule "O" (list "r"))) #t)
  (check-equal? (is-success? "Oronris" (rule "r" (list "a" "i" "o"))) #t)
  (check-equal? (is-success? "Oronris" (rule "i" (list "p" "w"))) #f)
  (check-equal? (is-success? "Gaermirath" (rule "a" (list "t" "l" "b"))) #f)

  (check-equal? (part-1 "inputs/quest-7-test.txt")
                (list "Oroneth")
                "part 1 test")
  (check-equal? (part-1 "inputs/quest-7.txt") (list "Azmirath") "part 1"))
