#lang racket

(require rackunit
         "quest-3.rkt")

(check-equal? (part-1 "inputs/quest-3-test.txt") 29 "Part 1 test input")
(check-equal? (part-1 "inputs/quest-3.txt") 3001 "Part 1 input")
