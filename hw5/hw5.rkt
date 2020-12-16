#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Problem 1
;; Write a function sequence that takes 3 arguments spacing, low, and high, all assumed to be numbers.
;; Further assume spacing is positive. sequence produces a list of numbers from low to high (including
;; low and possibly high) separated by spacing and in sorted order. Sample solution: 4 lines. Examples:
;;
;; | Call              |  Result         |
;; |-------------------|-----------------|
;; | (sequence 2 3 11) |  '(3 5 7 9 11)  |
;; | (sequence 3 3 8)  |  '(3 6)         |
;; | (sequence 1 3 2)  |  '()            |

(define (sequence spacing low high)
  (if (> low high)
      null
      (cons low (sequence spacing (+ low spacing) high))))
