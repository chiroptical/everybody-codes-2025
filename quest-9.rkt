#lang racket

(provide part-1
         part-1-serial)

(define (analyze-genome dad mom child)
  (for/fold ([acc (list 0 0)])
            ([d dad]
             [m mom]
             [c child]
             #:break (match acc
                       [#f #t]
                       [_ #f]))
    (let ([dad-match (char=? d c)]
          [mom-match (char=? m c)]
          [curr-dad (first acc)]
          [curr-mom (second acc)])
      (match (list dad-match mom-match)
        [(list #t #t) (list (+ curr-dad 1) (+ curr-mom 1))]
        [(list #t #f) (list (+ curr-dad 1) curr-mom)]
        [(list #f #t) (list curr-dad (+ curr-mom 1))]
        [(list #f #f) #f]))))

(define (get-parent-child-relationships parents ducks)
  (let* ([dad (first parents)]
         [dad-genome (hash-ref ducks dad)]
         [mom (second parents)]
         [mom-genome (hash-ref ducks mom)]
         [ducks-no-parents (hash-remove (hash-remove ducks dad) mom)])
    (for/fold ([acc (list)]) ([child-genome (hash-values ducks-no-parents)])
      (match (analyze-genome dad-genome mom-genome child-genome)
        [#f (cons 0 acc)]
        [(list d m) (cons (* d m) acc)]))))

(define (part-1 filename)
  (let* ([lines (file->lines filename)]
         [ducks (for/fold ([acc (make-immutable-hash)]) ([duck lines])
                  (match (string-split duck ":")
                    [(list hd tl) (hash-set acc (string->number hd) tl)]))]
         [potential-parents (combinations (hash-keys ducks) 2)]
         [futures (for/list ([parents potential-parents])
                    (future (thunk (get-parent-child-relationships parents
                                                                   ducks))))])
    (for/fold ([acc 0]) ([fut futures])
      (+ acc (foldl + 0 (touch fut))))))

(define (part-1-serial filename)
  (let* ([lines (file->lines filename)]
         [ducks (for/fold ([acc (make-immutable-hash)]) ([duck lines])
                  (match (string-split duck ":")
                    [(list hd tl) (hash-set acc (string->number hd) tl)]))]
         [potential-parents (combinations (hash-keys ducks) 2)])
    (for/fold ([acc 0]) ([parents potential-parents])
      (let ([relationships (get-parent-child-relationships parents ducks)])
        (+ acc (foldl + 0 relationships))))))
