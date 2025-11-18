#lang racket

(provide part-1)

(define (process-line row ln)
  (for/fold ([acc (set)])
            ([c ln]
             [col (in-naturals)])
    (match c
      [#\S (set-add acc (point row col))]
      [_ acc])))

(struct point (r c) #:transparent)

(define (read-notes file)
  (let* ([lines (file->lines file)]
         [sheep (for/fold ([acc (set)])
                          ([ln lines]
                           [idx (in-naturals)])
                  (let ([new-points (process-line idx ln)])
                    (set-union acc new-points)))])
    sheep))

(define (part-1 file)
  (read-notes file))
