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

; Problem 4
(define pows-of-two 
  (letrec
    ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 1))))

(assert (equal?
          (stream-for-k-steps pows-of-two 2)
          (list 1 2)))

(assert (equal?
          (stream-for-k-steps pows-of-two 1)
          '(1)))

(assert (equal?
          (stream-for-k-steps pows-of-two 0)
          '()))

; Problem 5
(assert (equal?
          (stream-for-k-steps funny-number-stream 0)
          '()))

(assert (equal?
          (stream-for-k-steps funny-number-stream 3)
          '(1 2 3)))

(assert (equal?
          (stream-for-k-steps funny-number-stream 6)
          '(1 2 3 4 5 -6)))

(assert (equal?
          (stream-for-k-steps funny-number-stream 13)
          '(1 2 3 4 5 -6 7 8 9 10 11 -12 13)))

; Problem 6
(assert (equal?
          (car (dan-then-dog)) "dan.jpg"))

(assert (equal?
          (car ((cdr (dan-then-dog)))) "dog.jpg"))

(assert (equal?
          (stream-for-k-steps dan-then-dog 3)
          '("dan.jpg" "dog.jpg" "dan.jpg")))

; Problem 7

(assert (equal?
         (car ((stream-add-one dan-then-dog)))
         '(1 . "dan.jpg")))

(assert (equal?
         (car ((cdr ((stream-add-one dan-then-dog)))))
         '(1 . "dog.jpg")))

(assert (equal?
          (stream-for-k-steps (stream-add-one dan-then-dog) 3)
          (list
            '(1 . "dan.jpg")
            '(1 . "dog.jpg")
            '(1 . "dan.jpg"))))

;; Problem 8
(assert (equal?
          (stream-for-k-steps (cycle-lists (list 1 2 3) (list 4 5)) 6)
          (list '(1 . 4)
                '(2 . 5)
                '(3 . 4)
                '(1 . 5)
                '(2 . 4)
                '(3 . 5))))

;; Problem 9
(assert (equal?
         (vector-assoc 0 #((0 . 1)))
         '(0 . 1)))

(assert (equal?
         (vector-assoc 0 #((1)))
         #f))

(assert (equal?
         (vector-assoc 1 #(1 2 3))
         #f))

(assert (equal?
         (vector-assoc 0 #(1 2 3))
         #f))

(assert (equal?
         (vector-assoc 2 #((1) (2) (3)))
         '(2)))

(displayln "HW5 test OK")
