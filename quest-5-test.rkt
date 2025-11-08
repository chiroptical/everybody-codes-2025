#lang racket

(require rackunit
         "quest-5.rkt")

(check-equal? (fishbone-insert (fishbone-init 1) 0)
              (fishbone 0 1 #f)
              "basic insert works")
(check-equal? (fishbone-insert (fishbone-init 1) 1) #f "basic insert works")
(check-equal? (fishbone-insert (fishbone 1 2 #f) 1) #f "basic insert works")
(check-equal? (fishbone-insert (fishbone 1 2 #f) 3)
              (fishbone 1 2 3)
              "basic insert works")
(check-equal? (fishbone-insert (fishbone 1 2 #f) 3)
              (fishbone 1 2 3)
              "basic insert works")

(check-equal? (fishbones-insert (list (fishbone 3 5 #f)) 7)
              (list (fishbone 3 5 7))
              "...")
(let ([in (list (fishbone 3 5 7) (fishbone #f 8 9) (fishbone #f 10 #f))]
      [out (list (fishbone 3 5 7) (fishbone 4 8 9) (fishbone #f 10 #f))])
  (check-equal? (fishbones-insert in 4) out "..."))
(let ([in (list (fishbone 5 10 #f))]
      [out (list (fishbone 5 10 #f) (fishbone #f 7 #f))])
  (check-equal? (fishbones-insert in 7) out "..."))
(let ([in (list (fishbone 5 10 #f) (fishbone #f 7 #f))]
      [out (list (fishbone 5 10 #f) (fishbone #f 7 8))])
  (check-equal? (fishbones-insert in 8) out "..."))
(let ([in (list (fishbone 5 10 #f) (fishbone #f 7 #f))]
      [out (list (fishbone 5 10 11) (fishbone #f 7 #f))])
  (check-equal? (fishbones-insert in 11) out "..."))
(let ([in (list (fishbone 5 10 #f) (fishbone #f 7 #f))]
      [out (list (fishbone 5 10 #f) (fishbone 6 7 #f))])
  (check-equal? (fishbones-insert in 6) out "..."))

(check-equal? (part-1 "inputs/quest-5-test.txt") "581078" "Part 1 test input")
(check-equal? (part-1 "inputs/quest-5.txt") "2348373452" "Part 1 input")
