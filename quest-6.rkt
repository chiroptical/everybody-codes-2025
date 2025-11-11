#lang racket

(provide part-1)

(struct state (mentors pairs))

(define (part-1 file)
  (let* ([mentor-list (call-with-input-file file (lambda (in) (read-line in)))]
         [final
          (foldl
           (lambda (c acc)
             (let ([current-mentors (state-mentors acc)]
                   [current-pairs (state-pairs acc)]
                   [is-sword-fighter (and (char-lower-case? c) (char=? c #\a))]
                   [upper (char-upcase c)])
               (if is-sword-fighter
                   (let ([add-pairs (hash-ref current-mentors upper 0)])
                     (state current-mentors (+ current-pairs add-pairs)))
                   ; TODO: not a clean fold, should use immutable hash map
                   (begin
                     (hash-update! current-mentors c (lambda (x) (+ x 1)) 0)
                     (state current-mentors current-pairs)))))
           (state (make-hash) 0)
           (string->list mentor-list))])
    (state-pairs final)))
