#lang racket

(require rackunit
         "quest-9.rkt")

(check-equal? (part-1 "inputs/quest-9-test.txt") 414 "part 1 test")
(check-equal? (part-1 "inputs/quest-9.txt") 414 "part 1")
