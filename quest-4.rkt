#lang racket

(provide part-1)

(struct state (gear-ratio previous-gear))

(define (part-1 file)
  (let* ([gears-str (file->lines file)]
         [gears (map string->number gears-str)]
         [final (match gears
                  [(cons hd tl)
                   (foldl (lambda (gear acc)
                            (let ([gear-num gear])
                              (state (* (/ (state-previous-gear acc) gear-num)
                                        (state-gear-ratio acc))
                                     gear-num)))
                          (state 1 hd)
                          tl)])])
    (exact-truncate (* 2025 (state-gear-ratio final)))))
