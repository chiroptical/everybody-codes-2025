#lang racket

(provide part-1
         fishbone
         fishbone-init
         fishbone-insert
         fishbones-insert)

(require racket/trace)
(require "util.rkt")

(struct fishbone (l c r) #:transparent)

(define (fishbone-init c)
  (fishbone #f c #f))

; Attempt to insert the value at the current level.
; If we can't, return #f, otherwise return the new fishbone
(define (fishbone-insert fb x)
  (match fb
    [(fishbone _ c _)
     #:when (= c x)
     #f]
    ; both sides false cases
    [(fishbone #f c #f)
     #:when (> x c)
     (fishbone #f c x)]
    [(fishbone #f c #f)
     #:when (< x c)
     (fishbone x c #f)]

    ; left is false
    [(fishbone #f c r)
     #:when (< x c)
     (fishbone x c r)]
    [(fishbone #f _c _r) #f]

    ; right is false
    [(fishbone l c #f)
     #:when (> x c)
     (fishbone l c x)]
    [(fishbone _l _c #f) #f]

    ; any other case
    [_ #f]))

(struct state (done fishbones))

(define (fishbones-insert fbs x)
  (let* ([out (foldl
               (lambda (fb st)
                 (match st
                   [(state #t curr)
                    (state #t
                           (append
                            curr
                            (list fb)))] ; value was inserted, just append rest
                   [(state #f curr)
                    (let ([try-insert (fishbone-insert fb x)])
                      (match try-insert
                        [#f (state #f (append curr (list fb)))]
                        [(fishbone a b c)
                         (state #t (append curr (list (fishbone a b c))))]))]))
               (state #f '())
               fbs)]
         [matched (state-done out)]
         [current (state-fishbones out)])
    (if matched
        current
        (append current (list (fishbone-init x))))))

(define (part-1 file)
  (let* ([bone-inp (first (file->lines file))]
         [bones (csv (second (string-split bone-inp ":")))]
         [fishbones (foldl (lambda (x acc)
                             (match acc
                               ['() (list (fishbone-init (string->number x)))]
                               [_ (fishbones-insert acc (string->number x))]))
                           '()
                           bones)])
    (foldr (lambda (x acc)
             (match x
               [(fishbone _ c _) (string-append (number->string c) acc)]))
           ""
           fishbones)))

(module+ test
  (require rackunit)

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
  (check-equal? (part-1 "inputs/quest-5.txt") "2348373452" "Part 1 input"))
