#lang racket
(require racket/random)  ; for random-ref
(require "hw7graphics.rkt")

(provide all-shapes rotations)
(provide tetris% board%)

(define all-colors
  (list "dark green" "dark blue" "dark red" "gold" "dark violet"
        "orange red" "gray"))

; module-exported function to compute all four rotations of a shape with no symmetries.
(define (rotations vector-of-points)
  (define (rotate90 point) (cons (- (cdr point)) (car point)))
  (define rotate180 (compose rotate90 rotate90))
  (define rotate270 (compose rotate180 rotate90))

  (for/vector ([f (in-list (list identity rotate90 rotate180 rotate270))])
    (vector-map f vector-of-points)))

;; A "shape" is a vector of vectors of points, representing all possible rotations of the shape.
;; A point is a pair of two ints.
;; Here is a list of all the classic tetris shapes (including their rotations as needed).
(define all-shapes
  (vector
   (vector (vector '(0 . 0) '(1 . 0) '(0 . 1) '(1 . 1)))        ; square (only needs one)
   (rotations (vector '(0 . 0) '(-1 . 0) '(1 . 0) '(0 . -1)))   ; T
   (vector (vector '(0 . 0) '(-1 . 0) '(1 . 0) '(2 . 0))        ; long (only needs two)
           (vector '(0 . 0) '(0 . -1) '(0 . 1) '(0 . 2)))
   (rotations (vector '(0 . 0) '(0 . -1) '(0 . 1) '(1 . 1)))    ; L
   (rotations (vector '(0 . 0) '(0 . -1) '(0 . 1) '(-1 . 1)))   ; inverted L
   (rotations (vector '(0 . 0) '(-1 . 0) '(0 . -1) '(1 . -1)))  ; S
   (rotations (vector '(0 . 0) '(1 . 0) '(0 . -1) '(-1 . -1))))); Z

(define (translate-point dx dy)
  (match-lambda [(cons x y) (cons (+ x dx) (+ y dy))]))

(define (translate-points dx dy)
  (curry vector-map (translate-point dx dy)))

; Represents the current piece, including its shape, position, rotation, and color.
(define piece%
  (class object%
    (super-new)
    (init-field all-rotations board)
    ;; invariant: in range [0..n), where n is (vector-length all-rotations)
    (define rotation-index (random (vector-length all-rotations)))

    (field [color (random-ref all-colors)])
    (field [(base-position position) '(5 . 0)])  ; (column . row) aka (x . y)

    (define (to-grid-space points)
      ((translate-points (car base-position) (cdr base-position)) points))

    (define (current-rotation)
      (vector-ref all-rotations rotation-index))

    (define/public (get-points)
      (to-grid-space (current-rotation)))

    ;; takes the intended movement in x, y and rotation and checks to see if the
    ;; movement is possible.  If it is, makes this movement and returns true.
    ;; Otherwise returns false.
    (define/public (move dx dy [drot 0])
      (define new-rotation-index (modulo (+ rotation-index drot) (vector-length all-rotations)))
      (define candidate-rotation
        ((translate-points dx dy)
         (to-grid-space (vector-ref all-rotations new-rotation-index))))
      ;; for each individual block in the piece, checks if the intended move
      ;; will put this block in an occupied space.
      ;; note fancy use of and: if sequence-andmap returns false, return false,
      ;; else, continue and do the rest of the "conjuncts". taking advantage,
      ;; of the fact that set! returns (void), which is truthy. (only #f is not truthy!)
      (and (sequence-andmap (λ (point) (send board empty-at? point)) candidate-rotation)
           (set! rotation-index new-rotation-index)
           (set! base-position ((translate-point dx dy) base-position))
           #t))

    (define/public (drop-by-one)
      (move 0 1))))

; Class responsible for the interaction between the pieces and the game itself
(define board%
  (class object%
    (super-new)
    (init-field game)
    (field [current-piece #f])
    (next-piece)
    (field [score 0]    ; current score
           [delay 500]  ; current delay
           [num-columns 10]
           [num-rows 27])

    (field [grid (build-vector num-rows (λ (_) (make-vector num-columns #f)))])

    ;; the game is over when there is a piece extending into the second row
    ;; from the top
    (define/public (game-over?)
      (sequence-ormap identity (vector-ref grid 1)))

    ; moves the current piece down by one (and returns #t), or,
    ; if that is not possible, stores the current piece in the grid,
    ; generates a new piece, and returns #f.
    (define/public (step)
      (define stepped (send current-piece drop-by-one))
      (unless stepped
        (store-current)
        (unless (game-over?)
          (next-piece)))
      stepped)

    ;; Moves the current piece in the given direction/rotation, if possible.
    ;; Return boolean indicating whether move succeeded.
    (define (move dx dy [drot 0])
      (and (not (game-over?))
           (send game running?)
           (begin0
               (send current-piece move dx dy drot)
             (send game refresh))))

    (define/public (move-left) (move -1 0))
    (define/public (move-right) (move 1 0))
    (define/public (rotate-clockwise) (move 0 0 1))
    (define/public (rotate-counter-clockwise) (move 0 0 -1))

    ;; drops the piece to the lowest location in the currently occupied columns.
    ;; Then replaces it with a new piece
    ;; Change the score to reflect the distance dropped.
    (define/public (drop-all-the-way)
      (when (send game running?)
        ; luv too l00p
        (do ()
            ((not (step)))
          (set! score (+ score 1)))
        (send game refresh)))

    (define/public (select-shape)
      (random-ref all-shapes))

    (define/public (next-piece)
      (set! current-piece
            (new piece% [all-rotations (select-shape)] [board this])))

    ;; Gets the information from the current piece about where it is and uses this
    ;; to store the piece on the board itself.  Then calls remove_filled.
    (define/public (store-current)
      (define points (send current-piece get-points))
      (for ([idx (in-range 4)])  ; teehee, I hope all the pieces have 4 blocks.
        (match-define (cons x y) (vector-ref points idx))
        (when (>= y 0)
          (vector-set! (vector-ref grid y) x (get-field color current-piece))))
      (remove-filled)
      (set! delay (max (- delay 2) 80)))

    ;; Takes a point and checks to see if it is in the bounds of the board and
    ;; currently empty.
    (define/public (empty-at? point)
      (match point
        [(cons x y)
         (and (>= x 0) (< x num-columns)
              (or (< y 1)
                  (and (< y num-rows)
                       (not (vector-ref (vector-ref grid y) x)))))]))

    ;; Removes all filled rows and replaces them with empty ones, dropping all rows
    ;; above them down each time a row is removed and increasing the score.
    (define/public (remove-filled)
      (for ([row-idx (in-range 2 (vector-length grid))])
        (define row (vector-ref grid row-idx))
        (when (sequence-andmap identity row)
          (vector-fill! row #f)
          (vector-copy! grid 1
                        grid 0 row-idx)
          (vector-set! grid 0 row)
          (set! score (+ score 10))))
      (send game refresh))))


(define tetris%
  (class object%
    (super-new)

    ;; Calls board's step method which moves a piece down and replaces it with a
    ;; new one when the old one can't move any more. Then refreshes the GUI and
    ;; calls run-game to reset the timer.
    (define (timer-callback)
      (send board step)
      (refresh)
      (run-game))

    (field [root (new tetris-root% [game this])]
           [timer (new tetris-timer% [notify-callback timer-callback])]
           [running #t]
           [board #f]  ; intialized by (set-board) below
           [block-size 25])  ; size (in pixels) of the little squares that make up the pieces

    ;;; Set up buttons and rest of GUI.
    (define global-pane (new tetris-vertical-pane% [alignment '(center center)] [parent root]))
    (define top-pane (new tetris-horizontal-pane% [alignment '(center center)] [parent global-pane]))
    (new tetris-button% [label "new game"] [parent top-pane] [callback (λ (b e) (new-game))])
    (new tetris-button% [label "pause"] [parent top-pane] [callback (λ (b e) (toggle-pause))])
    (new tetris-button% [label "quit"] [parent top-pane] [callback (λ (b e) (on-close) (exit))])

    (define score-pane (new tetris-horizontal-pane% [alignment '(center center)] [parent global-pane]))
    (new tetris-message% [label "Current Score:"] [parent score-pane])
    (field [score (new tetris-message% [label "-"] [parent score-pane] [auto-resize #t])])
    (set-board)

    ;; The canvas is the general-purposes drawing widget that will let us draw rectangles
    ;; so that we can visualize the board. See the paint method below.
    (field [canvas (new tetris-canvas%
                      [game this]
                      [parent global-pane]
                      [stretchable-width #f] [stretchable-height #f]
                      [min-width (* block-size (get-field num-columns board))]
                      [min-height (* block-size (get-field num-rows board))])])
    (send canvas set-canvas-background (make-color "light blue"))

    (new tetris-button% [label "⟳"] [parent global-pane] [callback (λ (b e) (send board rotate-clockwise))])

    (define arrow-pane (new tetris-horizontal-pane% [alignment '(center center)] [parent global-pane]))
    (new tetris-button% [label "left"] [parent arrow-pane] [callback (λ (b e) (send board move-left))])
    (new tetris-button% [label "drop"] [parent arrow-pane] [callback (λ (b e) (send board drop-all-the-way))])
    (new tetris-button% [label "right"] [parent arrow-pane] [callback (λ (b e) (send board move-right))])

    (new tetris-button% [label "⟲"] [parent global-pane] [callback (λ (b e) (send board rotate-counter-clockwise))])

    ;; Handle keypresses. Can be "augmented" by subclasses to handle further keys.
    (define/pubment (on-char event)
      (define keycode (send event get-key-code))
      (match keycode
        [#\n (new-game)]
        [#\p (toggle-pause)]
        [#\q (exit)]
        [(or #\a 'left) (send board move-left)]
        [(or #\d 'right) (send board move-right)]
        [(or #\s 'down) (send board rotate-counter-clockwise)]
        [(or #\w 'up) (send board rotate-clockwise)]
        [#\space (send board drop-all-the-way)]
        [_ (inner #f on-char event)]))

    (update-score)
    (send root show #t)
    (run-game)

    (define/public (set-board)
      (set! board (new board% [game this])))

    ;; Repeatedly calls itself (through timer-callback) so that the process is
    ;; fully automated. Checks if the game is over and if it isn't, resets the timer.
    (define/public (run-game)
      (when (and (not (send board game-over?))
                 running)
        (send timer stop)
        (send timer start (get-field delay board) #t)))

    (define/public (refresh)
      (update-score)
      (send canvas refresh-now))

    ; Starts the game over, replacing the old board and score.
    (define/public (new-game)
      (set-board)
      (set! running #t)
      (refresh)
      (run-game))

    ;; Uses racket/draw's drawing context to draw a block.
    (define/public (draw-block dc row-idx column-idx color)
      (send dc set-brush color 'solid)
      (send dc draw-rectangle (* block-size column-idx) (* block-size row-idx) block-size block-size))

    ;; Iterates over the board's grid plus the current piece, and draws all the
    ;; blocks with the right colors.
    ;; Called by tetris-canvas% (see on-paint in hw7graphics.rkt), which is
    ;; called indirectly through the refresh-now method call above.
    ;; dc is a racket/draw "drawing context" (see docs)
    (define/public (paint dc)
      (for ([row (in-vector (get-field grid board))]
            [row-idx (in-naturals)])
        (for ([maybe-color (in-vector row)]
              [column-idx (in-naturals)]
              #:when maybe-color)
          (draw-block dc row-idx column-idx maybe-color)))
      (define current-piece (get-field current-piece board))
      (define current-color (get-field color current-piece))
      (for ([point (send current-piece get-points)])
        (match-let ([(cons x y) point])
          (draw-block dc y x current-color))))

    ; pauses the game or resumes it
    (define/public (toggle-pause)
      (cond [running (send timer stop)
                     (set! running #f)]
            [else (set! running #t)
                  (run-game)]))

    (define/public (running?)
      running)

    ; the application will not exit cleanly if the timer is left running
    (define/public (on-close)
      (send timer stop))

    ; alters the displayed score to reflect what is currently stored in the board
    (define/public (update-score)
      (send score set-label (number->string (get-field score board))))))
