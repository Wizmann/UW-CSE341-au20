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

(define (stream-for-k-steps stream k)
  (if (> k 0)
    (cons
      (car (stream))
      (stream-for-k-steps (cdr (stream)) (- k 1)))
    null))

;; Problem 5
;; Write a stream funny-number-stream that is like the stream of natural numbers (i.e., 1, 2, 3, ...)
;; except numbers divisble by 6 are negated (i.e., 1, 2, 3, 4, 5, -6, 7, 8, 9, 10, 11, -12, 13, ...).
;; Remember a stream is a thunk that when called produces a pair. Here the car of the pair will be a'
;; number and the cdr will be another stream.

(define funny-number-stream
  (letrec 
    ([f (lambda (x) 
          (cons (if (= (remainder x 6) 0) (- x) x)
                (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; Problem 6
;; Write a stream dan-then-dog, where the elements of the stream alternate between the strings "dan.jpg"
;; and "dog.jpg" (starting with "dan.jpg"). More specifically, dan-then-dog should be a thunk that
;; when called produces a pair of "dan.jpg" and a thunk that when called produces a pair of "dog.jpg"
;; and a thunk that when called... etc.
;; Sample solution: 4 lines.

(define dan-then-dog
  (letrec
    ([f (lambda (x)
          (cons
            (list-nth-mod '("dan.jpg" "dog.jpg") x)
            (lambda () (f (bitwise-xor x 1)))))])
    (lambda () (f 0))))

;; Problem 7
;; Write a function stream-add-one that takes a stream s and returns another stream. If s would
;; produce v for its ith element, then (stream-add-one s) would produce the pair (1 . v) for its
;; i-th element.
;; Sample solution: 4 lines. Hint: Use a thunk that when called uses s and recursion.
;; Note: One of the provided tests uses (stream-add-one dan-then-dog) with place-repeatedly.

(define (stream-add-one stream)
  (lambda ()
      (cons (cons 1 (car (stream)))
            (stream-add-one (cdr (stream))))))

;; Problem 8
;; Write a function cycle-lists that takes two lists xs and ys and returns a stream. The lists may or
;; may not be the same length, but assume they are both non-empty. The elements produced by the
;; stream are pairs where the first part is from xs and the second part is from ys. The stream cycles
;; forever through the lists. For example, if xs is '(1 2 3) and ys is '("a" "b"), then the stream
;; would produce, (1 . "a"), (2 . "b"), (3 . "a"), (1 . "b"), (2 . "a"), (3 . "b"), (1 . "a"),
;; (2 . "b"), etc.
;; Sample solution is 6 lines and is more complicated than the previous stream problems. Hints: Use one
;; of the functions you wrote earlier. Use a recursive helper function that takes a number n and calls
;; itself with (+ n 1) inside a thunk.

(define (cycle-lists xs ys)
  (letrec
    ([f (lambda (xi yi)
      (cons
        (cons (list-ref xs xi) (list-ref ys yi))
        (lambda () (f (remainder (+ xi 1) (length xs)) (remainder (+ yi 1) (length ys))))))])
    (lambda () (f 0 0))))

;; Problem 9
;; Write a function vector-assoc that takes a value v and a vector vec. It should behave like Racket's
;; assoc library function except (1) it processes a vector (Racket's name for an array) instead of a list,
;; (2) it allows vector elements not to be pairs in which case it skips them, and (3) it always takes exactly
;; two arguments. Process the vector elements in order starting from 0. You must use library functions
;; vector-length, vector-ref, and equal?. Return #f if no vector element is a pair with a car field
;; equal to v, else return the first pair with an equal car field. Sample solution is 9 lines, using one local
;; recursive helper function.

(define (vector-assoc v vec)
  (letrec
    ([f (lambda(index)
          (cond [(>= index (vector-length vec)) #f]
                [(let ([cur (vector-ref vec index)])
                   (and (pair? cur) (= (car cur) v))) (vector-ref vec index)]
                [else (f (+ index 1))]))])
     (f 0)))

;; Problem 10
;; Write a function caching-assoc that takes a list xs and a positive number n and returns a function
;; that takes one argument v and returns the same thing that (assoc v xs) would return. However, you
;; should use an n-element cache of recent results to possibly make this function faster than just calling
;; assoc (if xs is long and a few elements are returned often). The cache should be a vector of length
;; n that is created by the call to caching-assoc and used-and-possibly-mutated each time the function
;; returned by caching-assoc is called.
;; The cache starts empty (all elements #f). When the function returned by caching-assoc is called, it
;; first checks the cache for the answer. If it is not there, it uses assoc and xs to get the answer and if
;; the result is not #f (i.e., xs has a pair that matches), it adds the pair to the cache before returning
;; (using vector-set!). The cache slots are used in a round-robin fashion: the first time a pair is added
;; to the cache it is put in position 0, the next pair is put in position 1, etc. up to position n - 1 and
;; then back to position 0 (replacing the pair already there), then position 1, etc.
;; Hints:
;; * In addition to a variable for holding the vector whose contents you mutate with vector-set!,
;; use a second variable to keep track of which cache slot will be replaced next. After modifying the
;; cache, increment this variable (with set!) or set it back to 0.
;; * To test your cache, it can be useful to add print expressions so you know when you are using the
;; cache and when you are not. But remove these print expressions before submitting your code.
;; * Sample solution is 15 lines.

(define (caching-assoc xs n)
  (let ([cache (make-vector n #f)]
        [cache-index 0])
    (lambda (v)
      (let ([cached (vector-assoc v cache)])
        (if (equal? cached #f)
          (let ([result (assoc v xs)])
            (if (equal? result #f)
              #f
              (begin
                (vector-set! cache cache-index (cons v result))
                (set! cache-index (remainder (+ cache-index 1) n))
                result)))
          (begin
            (cdr cached)))))))

;; Problem 11
;; Define a macro that is used like (while-greater e1 do e2) where e1 and e2 are expressions and
;; while-greater and do are syntax (keywords). The macro should do the following:
;; * It evaluates e1 exactly once.
;; * It evaluates e2 at least once.
;; * It keeps evaluating e2 until and only until the result is not a number greater than the result of
;; the evaluation of e1 .
;; * Assuming evaluation terminates, the result is #t.
;; * Assume e1 and e2 produce numbers; your macro can do anything or fail mysteriously otherwise.
;; Hint: Define and use a recursive thunk. Sample solution is 9 lines. Example:
;; (define a 7)
;; (while-greater 2 do (begin (set! a (- a 1)) (print "x") a))
;; (while-greater 2 do (begin (set! a (- a 1)) (print "x") a))
;; Evaluating the second line will print "x" 5 times and change a to be 2. So evaluating the third line
;; will print "x" 1 time and change a to be 1.
(define-syntax while-greater
  (syntax-rules (do)
    [(while-greater e1 do e2)
     (letrec ([v1 e1]
              [f (lambda ()
                   (begin (if (<= e2 v1) #t (f))))])
             (f))]))


;; Problem 12 (Challenge Problem)
;; Write cycle-lists-challenge. It should be equivalent to cycle-lists, but its implementation must be more
;; efficient. In particular, for each time the stream produces a new value, the code must perform only two
;; car operations and two cdr operations, including operations performed by any function calls.
;; So, for example, you cannot use length because it uses cdr multiple times to compute a list's length.

(define (cycle-lists-challenge xs ys)
  (letrec ([f (lambda (xs1 ys1)
                (cons
                  (cons (car xs1) (car ys1))
                  (lambda () (
                              let ([xtl (cdr xs1)]
                                   [ytl (cdr ys1)])
                              (f (if (null? xtl) xs xtl) (if (null? ytl) ys ytl))))))])
  (lambda () (f xs ys))))



;; Problem 13 (Challenge Problem)
;; Write caching-assoc-lru, which is like caching-assoc except it uses a policy
;; of "least recently used" for deciding which cache slot to replace. That is, when replacing a pair in
;; the cache, you must choose the pair that was least recently returned as an answer. Doing so requires
;; maintaining extra state.

(define (caching-assoc-lru xs n)
    (let ([cache (make-list n '(#f))])
      (lambda (x)
        (let ([cached (assoc x cache)])
          (if (equal? cached #f)
              (let ([result (assoc x xs)])
                (if (equal? result #f)
                    #f
                    (begin
                        (set! cache
                          (cons (cons x result) (take cache (- n 1))))
                        result)))
              (begin
                (set! cache (cons cached (dropf cache (lambda (c) (equal? x (car c))))))
                (cdr cached)))))))
