#lang racket

(require rackunit
         "quest-10.rkt")

(check-equal? (part-1 "inputs/quest-10-test.txt" 3) 26 "Part 1 test input")

; TODO: This is incorrect
(check-equal? (part-1 "inputs/quest-10.txt" 4) 140 "Part 1 input")
