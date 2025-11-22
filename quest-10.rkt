#lang racket

(provide part-1
         render-mat)

(define (process-line row ln)
  (for/fold ([sheep-set (set)]
             [dragon-set (set)]
             [_ 0])
            ([c ln]
             [col (in-naturals)])
    (match c
      [#\S
       (let ([new-sheep (set-add sheep-set (point row col))])
         (values new-sheep dragon-set col))]
      [#\D
       (let ([new-dragon (set-add dragon-set (point row col))])
         (values sheep-set new-dragon col))]
      [_ (values sheep-set dragon-set col)])))

(struct state (sheep dragon max-dims) #:transparent)

(define (make-initial-state)
  (state (set) (set) (point 0 0)))

(struct point (r c) #:transparent)

(define (read-notes file)
  (let* ([lines (file->lines file)]
         [initial-state (for/fold ([st (make-initial-state)])
                                  ([ln lines]
                                   [idx (in-naturals)])
                          (let ([curr-sheep (state-sheep st)]
                                [curr-dragon (state-dragon st)])
                            (let-values ([(new-sheep new-dragon max-col)
                                          (process-line idx ln)])
                              (state (set-union curr-sheep new-sheep)
                                     (set-union curr-dragon new-dragon)
                                     (point idx max-col)))))])
    initial-state))

(define knights-moves
  (list (point -2 1)
        (point -2 -1)
        (point -1 2)
        (point -1 -2)
        (point 1 2)
        (point 1 -2)
        (point 2 1)
        (point 2 -1)))

(define (valid-move start move max-dims)
  (let* ([start-r (point-r start)]
         [start-c (point-c start)]
         [move-r (point-r move)]
         [move-c (point-c move)]
         [max-r (point-r max-dims)]
         [max-c (point-c max-dims)]
         [final (point (+ start-r move-r) (+ start-c move-c))])
    (match final
      [(point r c)
       #:when (or (> r max-r) (> c max-c) (< r 0) (< c 0))
       #f]
      [_ final])))

(define (make-moves start max-dims)
  (for/fold ([acc (set start)]) ([move knights-moves])
    (match (valid-move start move max-dims)
      [#f acc]
      [p (set-add acc p)])))

(define (part-1 file n)
  (let* ([initial-state (read-notes file)]
         [max-dims (state-max-dims initial-state)]
         [sheep (state-sheep initial-state)]
         [init-dragons (state-dragon initial-state)]
         ; TODO: plot final dragons and see what is missing
         [final-dragons (for/fold ([acc init-dragons]) ([_ (range n)])
                          (foldl (lambda (dragon final)
                                   (set-union (make-moves dragon max-dims)
                                              final))
                                 acc
                                 (set->list acc)))]
         [intersection (set-intersect sheep final-dragons)])
    (set-count intersection)))

(define (render-mat mat dims)
  (match dims
    [(point rows cols)
     (for* ([r rows]
            [c cols])
       (let ([display-char (if (set-member? mat (point r c)) #\X #\.)])
         (display display-char)
         (if (= c (- cols 1))
             (display "\n")
             #f)))]))
