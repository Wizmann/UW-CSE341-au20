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

;; Problem 2
;; Write a function string-append-map that takes a list of strings xs and a string suffix and returns a
;; list of strings. Each element of the output should be the corresponding element of the input appended
;; with suffix (with no extra space between the element and suffix). You must use Racket-library
;; functions map and string-append. Sample solution: 2 lines.

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; Problem 3
;; Write a function list-nth-mod that takes a list xs and a number n. If the number is negative,
;; terminate the computation with (error "list-nth-mod: negative number"). Else if the list is
;; empty, terminate the computation with (error "list-nth-mod: empty list"). Else return the ith
;; element of the list where we count from zero and i is the remainder produced when dividing n by the
;; list's length. Library functions length, remainder, car, and list-tail are all useful
;; - see the Racket documentation.
;; Sample solution is 6 lines.

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "negative number")]
        [(= (length xs) 0) (error "list-nth-mod: empty list")]
        [else (car (list-tail xs (remainder n (length xs))))]))

;; Problem 4
;; Write a function stream-for-k-steps that takes a stream s and a number k. It returns a list holding
;; the first k values produced by s in order. Assume k is non-negative. Sample solution: 5 lines.
;; Note: You can test your streams with this function instead of the graphics code.

;; Problem 5
;; Write a stream funny-number-stream that is like the stream of natural numbers (i.e., 1, 2, 3, ...)
;; except numbers divisble by 6 are negated (i.e., 1, 2, 3, 4, 5, -6, 7, 8, 9, 10, 11, -12, 13, ...).
;; Remember a stream is a thunk that when called produces a pair. Here the car of the pair will be a'
;; number and the cdr will be another stream.
