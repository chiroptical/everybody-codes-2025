#lang racket

(require rackunit
         "quest-1.rkt")

(check-equal? (part-1 "inputs/quest-1-test.txt") "Fyrryn" "Part 1 test input")
(check-equal? (part-1 "inputs/quest-1.txt") "Caelzar" "Part 1 input")

(check-equal? (part-2 "inputs/quest-1-test.txt") "Elarzris" "Part 2 test input")
(check-equal? (part-2 "inputs/quest-1.txt") "Elarzris" "Part 2 test input")
