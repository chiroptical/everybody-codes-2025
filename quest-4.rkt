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

(module+ test
  (require rackunit)

  (check-equal? (part-1 "inputs/quest-4-test-1.txt")
                32400
                "Part 1 test input 1")
  (check-equal? (part-1 "inputs/quest-4-test-2.txt")
                15888
                "Part 1 test input 2")
  (check-equal? (part-1 "inputs/quest-4.txt") 10125 "Part 1"))
