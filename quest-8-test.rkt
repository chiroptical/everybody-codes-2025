#lang racket

(require rackunit
         "quest-8.rkt")

(check-equal? (part-1 "inputs/quest-8-test.txt" 8) 4 "part 1 test")
(check-equal? (part-1 "inputs/quest-8.txt" 32) 58 "part 1")

(check-equal? (overlaps? (vec 1 5) (vec 2 6)) #t "overlaps work")
(check-equal? (overlaps? (vec 1 5) (vec 2 5)) #f "overlaps work")
(check-equal? (overlaps? (vec 1 5) (vec 5 8)) #f "overlaps work")
(check-equal? (overlaps? (vec 1 5) (vec 4 8)) #t "overlaps work")
(check-equal? (overlaps? (vec 1 5) (vec 6 8)) #f "overlaps work")

(check-equal? (overlaps? (vec 2 6) (vec 1 4)) #t "overlaps work")
(check-equal? (overlaps? (vec 2 5) (vec 1 4)) #t "overlaps work")

(check-equal? (overlaps? (vec 1 5) (vec 3 7)) #t "overlaps work")
(check-equal? (overlaps? (vec 1 4) (vec 3 7)) #t "overlaps work")
(check-equal? (overlaps? (vec 2 5) (vec 3 7)) #t "overlaps work")
(check-equal? (overlaps? (vec 2 6) (vec 3 7)) #t "overlaps work")
(check-equal? (overlaps? (vec 6 8) (vec 3 7)) #t "overlaps work")

(check-equal? (part-2 "inputs/quest-8-test-2.txt") 21 "part 2 test")
(check-equal? (part-2 "inputs/quest-8-2.txt") 2925920 "part 2")

(check-equal? (drop-at (list 1 2 3) 1) (list 1 3) "...")

(check-equal? (part-3 "inputs/quest-8-test-3.txt") 7 "part 3 test")
; TODO: This is slow, the value 2786 is wrong
; (check-equal? (part-3 "inputs/quest-8-3.txt") 2786 "part 3")
