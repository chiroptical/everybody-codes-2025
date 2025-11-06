#lang racket

(provide csv)

(define (csv in)
  (string-split in ","))
