#lang racket

(provide part-1)

(define (shift f ducks from)
  (let* ([to (f from 1)]
         [ducks-from (hash-ref ducks from)]
         [ducks-to (hash-ref ducks to)])
    (if (> ducks-from ducks-to)
        (hash-set (hash-set ducks from (- ducks-from 1)) to (+ ducks-to 1))
        #f)))

(define (shift-right ducks from)
  (shift + ducks from))

(define (shift-left ducks from)
  (shift - ducks from))

(define (right-round ducks num-ducks)
  (for/fold ([acc ducks]) ([idx (in-range (- num-ducks 1))])
    (match (shift-right acc idx)
      [#f acc]
      [d d])))

(define (left-round ducks num-ducks)
  (for/fold ([acc ducks]) ([idx (in-range 1 num-ducks)])
    (match (shift-left acc idx)
      [#f acc]
      [d d])))

(define (right acc prev num-ducks)
  (let* ([next (right-round acc num-ducks)]
         [change-direction (equal? prev next)])
    (match change-direction
      [#f (values next next 'right)]
      [#t (values next (make-immutable-hash) 'left)])))

(define (left acc prev num-ducks)
  (let* ([next (left-round acc num-ducks)]
         [change-direction (equal? prev next)])
    (match change-direction
      [#f (values next next 'left)]
      [#t (values next (make-immutable-hash) 'right)])))

(define (checksum ducks num-ducks)
  (for/fold ([acc 0]) ([idx (in-range num-ducks)])
    (+ acc (* (+ idx 1) (hash-ref ducks idx)))))

(define (part-1 filename)
  (let* ([lines (file->lines filename)]
         [ducks (for/fold ([acc (make-immutable-hash)])
                          ([l lines]
                           [idx (in-naturals)])
                  (hash-set acc idx (string->number l)))]
         [num-ducks (hash-count ducks)]
         [final-ducks (match/values (for/fold ([acc ducks]
                                               [prev (make-immutable-hash)]
                                               [direction 'right])
                                              ([_ (in-range 11)])
                                      (match direction
                                        ['right (right acc prev num-ducks)]
                                        ['left (left acc prev num-ducks)]))
                                    [(final _ _) final])])
    (checksum final-ducks num-ducks)))

(module+ test
  (require rackunit)

  (check-equal? (shift-right (hash 0 1 1 0) 0)
                (hash 0 0 1 1)
                "shift-right works")

  (check-equal? (shift-right (hash 0 0 1 1) 0) #f "shift right fails")

  (check-equal? (shift-left (hash 0 0 1 1) 1) (hash 0 1 1 0) "shift left works")

  (check-equal? (shift-left (hash 0 2 1 1) 1) #f "shift-left fails")

  (check-equal? (let ([x (hash 0 9 1 1 2 1 3 4 4 9 5 6)])
                  (match/values (right x x 6) [(o _ _) o]))
                (hash 0 8 1 1 2 2 3 4 4 8 5 7)
                "second round should work")

  (check-equal? (let ([x (hash 0 8 1 1 2 2 3 4 4 8 5 7)])
                  (match/values (right x x 6) [(o _ _) o]))
                (hash 0 7 1 2 2 2 3 4 4 7 5 8)
                "second round should work")

  (check-equal? (part-1 "inputs/quest-11-test.txt") 109 "part 1 test")
  (check-equal? (part-1 "inputs/quest-11.txt") 312 "part 1"))
