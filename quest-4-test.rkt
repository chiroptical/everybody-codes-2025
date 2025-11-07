#lang racket

(require rackunit
         "quest-4.rkt")

(check-equal? (part-1 "inputs/quest-4-test-1.txt") 32400 "Part 1 test input 1")
(check-equal? (part-1 "inputs/quest-4-test-2.txt") 15888 "Part 1 test input 2")
(check-equal? (part-1 "inputs/quest-4.txt") 10125 "Part 1")
