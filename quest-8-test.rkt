#lang racket

(require rackunit
         "quest-8.rkt")

(check-equal? (part-1 "inputs/quest-8-test.txt" 8) 4 "part 1 test")
(check-equal? (part-1 "inputs/quest-8.txt" 32) 58 "part 1")
