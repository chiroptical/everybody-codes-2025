#lang racket

(require "util.rkt")

(provide part-1)

(define (parse-lines lines)
  (for/fold ([acc (make-immutable-hash)])
            ([l lines]
             [row (in-naturals)])
    (for/fold ([inner-acc acc])
              ([bomb l]
               [col (in-naturals)])
      (hash-set inner-acc (point row col) (char->number bomb)))))

(define (move bombs from)
  (let ([moves (list (point -1 0) (point 1 0) (point 0 -1) (point 0 1))]
        [orig (hash-ref bombs from)])
    (for/fold ([acc (set)]) ([dxy moves])
      (let* ([visit-point (add-points from dxy)]
             [comp (hash-ref bombs visit-point (lambda () #f))])
        (match comp
          [#f acc]
          [next
           (if (>= orig next)
               (set-add acc visit-point)
               acc)])))))

(define (perform-moves bombs visited)
  (for/fold ([acc visited]) ([v (set->list visited)])
    (let ([add-points (move bombs v)]) (set-union add-points acc))))

; TODO: Could be more efficient, currently we attempt to perform a bunch of duplicative checks
; because we just loop over `visited` each time. Ideally, we keep track of new additions to `visited`
; and use those points to make moves.
(define (part-1 filename)
  (let*-values ([(lines) (file->lines filename)]
                [(bombs) (parse-lines lines)]
                [(moves _) (for/fold ([visited (set (point 0 0))]
                                      [done #f])
                                     ([_ (in-naturals)])
                             #:break done
                             (let ([next (perform-moves bombs visited)])
                               (if (equal? next visited)
                                   (values next #t)
                                   (values next #f))))])
    (set-count moves)))

(module+ test
  (require rackunit)

  (check-equal? (part-1 "inputs/quest-12-test.txt") 16 "part 1 test")
  (check-equal? (part-1 "inputs/quest-12.txt") 234 "part 1"))
