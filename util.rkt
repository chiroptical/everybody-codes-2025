#lang racket

(provide csv
         todo)

(define (csv in)
  (string-split in ","))

(define (todo)
  (error "not implemented yet"))
