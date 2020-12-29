#lang racket

;; This is the only file you turn in, so do not modify the other files as
;; part of your solution.

(require "hw7provided.rkt")
(provide my-tetris%)
(require racket/random)  ; for random-ref
; Uncomment this line if you do the challenge problem
;(provide my-tetris-challenge%)

;; Edit the two classes below to implement your enhancements.

(define my-tetris%
  (class tetris%
    (super-new)
    (inspect #f)

    (define/override (set-board)
      (set! board (new my-board% [game this])))

    ; 1. In your game, the player can press the ’u’ key to make the piece that is
    ; falling rotate 180 degrees.
    ; (Note it is normal for this to make some pieces appear to move slightly.)
    (inherit-field board)

    (define/augment (on-char event)
      (define keycode (send event get-key-code))
      (displayln keycode)
      (match keycode
        [#\u (begin
               (send board rotate-clockwise)
               (send board rotate-clockwise))]
        [#\c (send board set-is-cheating! #t)]
        [_ (inner #f on-char event)]))

))

(define my-board%
  (class board%
    (field [is-cheating #f])
    (super-new)

    (inherit-field current-piece grid delay score)
    (inherit remove-filled)

    (define/public (set-is-cheating! flag) (set! is-cheating flag))

    (define/public (cheat) 
      (let ([flag (and is-cheating (>= score 100))])
        (set-is-cheating! #f)
        (if flag (set! score (- score 100)) #f)
        flag))

    ; 2. In your game, instead of the pieces being randomly (and uniformly) chosen from
    ; the 7 classic pieces, the pieces are randomly (and uniformly) chosen from 10 pieces.
    ; They are the classic 7 and these 3:
    ; 
    ; OO                          O
    ; OOO         OOOOO           OO
    ; 
    ; The initial rotation for each piece is also chosen randomly.

    ; 3. In your game, the player can press the ’c’ key to cheat: If the score is less than 100,
    ; nothing happens.
    ; Else the player loses 100 points (cheating costs you) and the next piece that appears will be:
    ; 
    ;     O  (a single block)
    ;
    ; The piece after is again chosen randomly from the 10 above (unless, of course, the player
    ; hits ’c’ while; the “cheat piece” is falling and still has a large enough score).
    ; Hitting ’c’ multiple times while a single piece is falling should behave no differently
    ; than hitting it once.


    (define/override (select-shape)
      (if (cheat)
        (vector (vector '(0 . 0)))
        (random-ref 
          (vector-append all-shapes
            (vector
              (vector (vector '(0 . 0) '(-1 . 0) '(1 . 0) '(2 . 0) '(-2 . 0))
                      (vector '(0 . 0) '(0 . -1) '(0 . 1) '(0 . 2) '(0 . -2)))     ; OOOOO

              (rotations (vector '(0 . 0) '(-1 . 0) '(1 . 0) '(0 . 1) '(-1 . 1)))  ; OO
                                                                                   ; OOO
                                                                                   
              (rotations (vector '(0 . 0) '(1 . 0) '(0 . 1)))                      ; O
                                                                                   ; OO
          )))))
        
    (define/override (store-current)
      (define points (send current-piece get-points))
      (let ([blocks (vector-length points)])
        (for ([idx (in-range blocks)])  ; teehee, I hope all the pieces have 4 blocks.
          (match-define (cons x y) (vector-ref points idx))
          (when (>= y 0)
            (vector-set! (vector-ref grid y) x (get-field color current-piece))))
        (remove-filled)
        (set! delay (max (- delay 2) 80))))
))
