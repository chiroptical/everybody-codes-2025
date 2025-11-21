#lang racket

(require benchmark)
(require plot)
(require "quest-9.rkt")

(time (part-1-serial "inputs/quest-9-2.txt"))
(time (part-1 "inputs/quest-9-2.txt"))

(define benchmark-results
  (run-benchmarks (list 'serial 'parallel)
                  (list (list "inputs/quest-9-2.txt"))
                  (lambda (op input)
                    (let ([fn (match op
                                ['serial (lambda (i) (part-1-serial i))]
                                ['parallel (lambda (i) (part-1 i))])])
                      (fn input)))
                  #:num-trials 30
                  #:extract-time 'delta-time))

(define renderer
  (render-benchmark-alts (list "inputs/quest-9-2.txt") benchmark-results))

(plot renderer #:out-file "quest-9.png")
