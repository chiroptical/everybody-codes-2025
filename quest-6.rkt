#lang racket

(provide part-1
         part-2
         sized
         sized-append
         part-3)

(struct state (mentors pairs))

(define (part-1 file)
  (let* ([mentor-list (call-with-input-file file (lambda (in) (read-line in)))]
         [final
          (foldl
           (lambda (c acc)
             (let ([current-mentors (state-mentors acc)]
                   [current-pairs (state-pairs acc)])
               (match c
                 [x
                  #:when (char=? x #\A)
                  (state (hash-update current-mentors x (lambda (y) (+ y 1)) 0)
                         current-pairs)]
                 [x
                  #:when (char=? x #\a)
                  (let ([add-pairs (hash-ref current-mentors #\A 0)])
                    (state current-mentors (+ current-pairs add-pairs)))]
                 [_ acc])))
           (state (make-immutable-hash) 0)
           (string->list mentor-list))])
    (state-pairs final)))

(define (part-2 file)
  (let* ([mentor-list (call-with-input-file file (lambda (in) (read-line in)))]
         [final
          (foldl
           (lambda (c acc)
             (let ([current-mentors (state-mentors acc)]
                   [current-pairs (state-pairs acc)])
               (match c
                 [x
                  #:when (char-upper-case? x)
                  (state (hash-update current-mentors x (lambda (y) (+ y 1)) 0)
                         current-pairs)]
                 [x
                  #:when (char-lower-case? x)
                  (let* ([upper (char-upcase x)]
                         [add-pairs (hash-ref current-mentors upper 0)])
                    (state current-mentors (+ current-pairs add-pairs)))])))
           (state (make-immutable-hash) 0)
           (string->list mentor-list))])
    (state-pairs final)))

; left and right can contain up to 1000 people
; focus contains the current character
(struct zipper (left focus right))

(require racket/generator)

; TODO: part-3
; https://docs.racket-lang.org/reference/treelist.html
;
; A.................
; ^^               ^
;  |               | 1000
;
; Initial zipper, generator grabs the next character to make,
;
;   .a................
;   ^ ^              ^
; 1 | |              | 1000
;
; - file forms an infinite character generator
; - read 1001 characters from the generator, forms initial treelist
; - read subsequent character, modify tree list
; - do tent calculation for left and right tree
; - add to pairs
; - note: the right side of the zipper needs to contract past num-iterations
(define (part-3 file)
  (let* ([mentor-chars (call-with-input-file file (lambda (in) (read-line in)))]
         [num-chars (length mentor-chars)]
         [num-iterations (* 1000 num-chars)]
         [mentor-seq (in-string mentor-chars)]
         [gen (sequence->repeated-generator mentor-seq)]
         ; TODO: get 1001 characters, build zipper
         [initial-state 0])
    (foldl (lambda (idx acc)
             (let (; TODO: we only pull from `gen` if we haven't reached the end
                   [should-gen #f]
                   [c (gen)])
               acc))
           initial-state
           (range 0 num-iterations))))
