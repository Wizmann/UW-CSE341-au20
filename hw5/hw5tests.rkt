#lang racket

(require "hw5.rkt")

(define-syntax-rule (assert expr)
  (unless expr
    (error 'assert "assertion failed: ~s" (quote expr))))


; Problem 1
(assert (equal? (sequence 2 3 11) '(3 5 7 9 11)))
(assert (equal? (sequence 3 3 8) '(3 6)))
(assert (equal? (sequence 1 3 2) '()))

; Problem 2
(assert (equal? (string-append-map '("foo" "bar" "baz") "123")
                '("foo123" "bar123" "baz123")))
(assert (equal? (string-append-map '() "123") '()))

; Problem 3
(assert (equal? (list-nth-mod '(1 2 3) 3)
                1))

(with-handlers
  ([exn:fail? (lambda (exn)
                (assert (equal? (exn-message exn) "negative number")))])
  (list-nth-mod '(1 2 3) -1))

(with-handlers
  ([exn:fail? (lambda (exn)
                (assert
                  (equal? (exn-message exn) "list-nth-mod: empty list")))])
  (list-nth-mod '() 3))


(displayln "HW5 test OK")
