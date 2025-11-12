#lang racket

(require rackunit
         "quest-7.rkt")

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

(check-equal? (part-1 "inputs/quest-7-test.txt") (list "Oroneth") "part 1 test")
(check-equal? (part-1 "inputs/quest-7.txt") (list "Azmirath") "part 1")
