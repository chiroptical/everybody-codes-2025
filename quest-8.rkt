#lang racket

(provide part-1
         part-2
         part-3
         overlaps?
         vec
         drop-at)

(require "util.rkt")

(define (read-notes file)
  (call-with-input-file file
                        (lambda (in)
                          (let* ([names-in (read-line in)]
                                 [as-csv (csv names-in)])
                            (map string->number as-csv)))))

(require racket/list/grouping)

(define (part-1 file n)
  (let ([notes (read-notes file)]
        [midpoint (/ n 2)])
    (foldl (lambda (window acc)
             (match window
               [(list x y)
                (if (= midpoint (abs (- x y)))
                    (+ 1 acc)
                    acc)]))
           0
           (windows 2 1 notes))))

(struct vec (lower upper) #:transparent)

(struct state (prev knots) #:transparent)

(define (overlaps? v1 v2)
  (let ([x (vec-lower v1)]
        [y (vec-upper v1)]
        [a (vec-lower v2)]
        [b (vec-upper v2)])
    (or (and (> a x) (< a y) (> b y)) (and (> x a) (< x b) (> y b)))))

(define (next-state vec s)
  (let* ([prev (state-prev s)]
         [knots (state-knots s)]
         [new-knots (foldl (lambda (p-vec acc)
                             (if (overlaps? p-vec vec)
                                 (+ acc 1)
                                 acc))
                           knots
                           prev)])
    (state (append prev (list vec)) new-knots)))

(define (make-vecs x)
  (match x
    [(list y z)
     (if (> y z)
         (vec z y)
         (vec y z))]))

(define (part-2 file)
  (let* ([notes (read-notes file)]
         [vecs (map make-vecs (windows 2 1 notes))]
         [new-state (foldl next-state (state (list) 0) vecs)])
    (state-knots new-state)))

; TODO: Should probably put this in utils with a contract,
; - (> idx 0)
; - lst is non-empty
; - idx is not greater than length of list
(define (drop-at lst idx)
  (struct drop-state (idx lst))
  (let ([st (foldl (lambda (x st)
                     (let ([curr (drop-state-idx st)]
                           [ls (drop-state-lst st)])
                       (if (= curr idx)
                           (drop-state (+ curr 1) ls)
                           (drop-state (+ curr 1) (append ls (list x))))))
                   (drop-state 0 (list))
                   lst)])
    (drop-state-lst st)))

(define (solve perms)
  (argmax identity
          (for/list ([i (range (length perms))])
            (let* ([vec (list-ref perms i)]
                   [rest (drop-at perms i)]
                   [st (next-state vec (state rest 1))])
              (state-knots st)))))

(define (part-3 file)
  (let* ([notes (read-notes file)]
         [vecs (map make-vecs (windows 2 1 notes))])
    (solve vecs)))

(module+ test
  (require rackunit)

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
  )
