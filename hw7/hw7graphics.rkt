#lang racket
(require racket/gui)

; This module provides wrapped access to the racket/gui library.
; The autograder will swap it out for a different, non-gui backend.

(provide tetris-root% tetris-timer% tetris-canvas% tetris-button% tetris-message%)
(provide tetris-horizontal-pane% tetris-vertical-pane%)
(provide make-color)

(define tetris-root%
  (class frame%
    (super-new [label "Tetris"])
    (init-field game)
    (define/override (on-subwindow-char receiver event)
      (or (send game on-char event)
          (super on-subwindow-char receiver event)))

    (define/augment (on-close)
      (send game on-close))))

(define tetris-timer%
  (class timer%
    (super-new)))

(define tetris-canvas%
  (class canvas%
    (init-field game)
    (super-new)
    (define/override (on-paint)
      (send game paint (send this get-dc)))))

(define tetris-message%
  (class message%
    (super-new)))

(define tetris-button%
  (class button%
    (super-new)))

(define tetris-horizontal-pane%
  (class horizontal-pane%
    (super-new)))

(define tetris-vertical-pane%
  (class vertical-pane%
    (super-new)))

(define (make-color color-name)
  (make-object color% color-name))
