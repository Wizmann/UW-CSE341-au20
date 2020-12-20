#lang racket

(require "hw7provided.rkt")
(require "hw7assignment.rkt")

(define mode (make-parameter 'enhanced))

(define (top-level)
  (command-line
   #:program "hw7runner"
   #:once-any
   ("--original" "Run the provided (base) game."
                 (mode 'original))
   ("--enhanced" "Run your enhanced version of the game game."
                 (mode 'enhanced))
   ; Uncomment this line if you do the challenge problem
   #;("--challenge" "Run your challenge problem version of the game game."
                    (mode 'challenge)))

  (println (mode))

  (match (mode)
    ['original (new tetris%)]
    ['enhanced (new my-tetris%)]
    ; Uncomment this line if you do the challenge problem
    #;['challenge (new my-tetris-challenge%)]))

(module* main #f
  (define the-game (top-level)))
