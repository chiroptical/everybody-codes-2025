#lang racket

(provide part-1)

(struct duck-seq (x y z))

; TODO: This solution really sucks
; It would probably be better to have a dictionary of `number -> sequence`
; That way parents and children have numbers. There is probably a much better
; representation of data here.
; For example, if you get all the keys you can form all the possible
; parent combinations. Then, for each parent combination look for valid children.
; That task can be completed in parallel.
(define (get-sequence ln)
  (let ([parts (string-split ln ":")])
    (match parts
      [(list _hd ... tl) tl])))

(define (read-notes file)
  (call-with-input-file
   file
   (lambda (test-in)
     (let ([x (read-line test-in)]
           [y (read-line test-in)]
           [z (read-line test-in)])
       (duck-seq (get-sequence x) (get-sequence y) (get-sequence z))))))

(define (get-matches s x y z)
  (let* ([xy (if (char=? x y)
                 (set 'x 'y)
                 (set))]
         [xz (if (char=? x z)
                 (set 'x 'z)
                 (set))]
         [yz (if (char=? y z)
                 (set 'y 'z)
                 (set))])
    (set-intersect s (set-union xy xz yz))))

(define (determine-child ducks)
  (for/fold ([children (set 'x 'y 'z)])
            ([x (duck-seq-x ducks)]
             [y (duck-seq-y ducks)]
             [z (duck-seq-z ducks)]
             #:break (= 1 (set-count children)))
    (match children
      [s (get-matches s x y z)])))

(define (determine-matches ducks parent)
  (let*-values ([(child p1 p2)
                 (match parent
                   ['x (values duck-seq-x duck-seq-y duck-seq-z)]
                   ['y (values duck-seq-y duck-seq-x duck-seq-z)]
                   ['z (values duck-seq-z duck-seq-x duck-seq-y)])]
                [(childs p1s p2s) (values (child ducks) (p1 ducks) (p2 ducks))])
    (for/fold ([acc (list 0 0)])
              ([c childs]
               [o p1s]
               [t p2s])
      (match acc
        [(list a b)
         #:when (and (char=? c o) (char=? c t))
         (list (+ 1 a) (+ 1 b))]
        [(list a b)
         #:when (char=? c o)
         (list (+ 1 a) b)]
        [(list a b)
         #:when (char=? c t)
         (list a (+ 1 b))]
        [_ acc]))))

(define (part-1 file)
  (let* ([ducks (read-notes file)]
         [child (set-first (determine-child ducks))]
         [matches (determine-matches ducks child)])
    (match matches
      [(list x y) (* x y)])))
