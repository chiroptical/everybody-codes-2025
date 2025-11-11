#lang racket

(require rackunit
         "quest-6.rkt")

; AaAaa 1 2 2
(check-equal? (part-1 "inputs/quest-6-test.txt") 5 "Part 1 test input")
(check-equal? (part-1 "inputs/quest-6.txt") 172 "Part 1 input")

(check-equal? (part-2 "inputs/quest-6-test.txt") 11 "Part 2 test input")
(check-equal? (part-2 "inputs/quest-6-part-2.txt") 3920 "Part 2 input")

(check-equal? (sized-append 2 (vector 1) 2) (vector 1 2) "sized append works")
(check-equal? (sized-append 2 (vector 1 2) 3)
              (vector 2 3)
              "sized append works, hits limit")

(check-equal? (part-3 "inputs/quest-6-test.txt") '() "...")
