#lang racket

(require "hw5.rkt")

(define-syntax-rule (assert expr)
  (unless expr
    (error 'assert "assertion failed: ~s" (quote expr))))


(assert (equal? (sequence 2 3 11) '(3 5 7 9 11)))
(assert (equal? (sequence 3 3 8) '(3 6)))
(assert (equal? (sequence 1 3 2) '()))
