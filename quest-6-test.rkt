#lang racket

(require rackunit
         "quest-6.rkt")

; AaAaa 1 2 2
(check-equal? (part-1 "inputs/quest-6-test.txt") 5 "Part 1 test input")
(check-equal? (part-1 "inputs/quest-6.txt") 172 "Part 1 test input")
