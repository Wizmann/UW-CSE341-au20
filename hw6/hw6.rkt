;; CSE341, Programming Languages, Homework 6

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct isgreater (e1 e2)    #:transparent) ;; if e1 > e2 then 1 else 0
(struct ifnz (e1 e2 e3) #:transparent) ;; if not zero e1 then e2 else e3
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair   (e1 e2) #:transparent) ;; make a new pair
(struct first   (e)     #:transparent) ;; get first part of a pair
(struct second  (e)     #:transparent) ;; get second part of a pair
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 2

;; (a) Write a Racket function racketlist->mupllist that takes a Racket list (presumably of mupl
;; values but that will not affect your solution) and produces an analogous mupl list with the same
;; elements in the same order.
(define (racketlist->mupllist items)
  (if (null? items)
      munit
      (apair (car items) (racketlist->mupllist (cdr items)))))

;; (b) Write a Racket function mupllist->racketlist that takes a mupl list (presumably of mupl
;; values but that will not affect your solution) and produces an analogous Racket list (of mupl
;; values) with the same elements in the same order.
(define (mupllist->racketlist items)
  (if (apair? items)
      (cons (apair-e1 items) (mupllist->racketlist (apair-e2 items)))
      null))

;; Problem 3

;; Implementing the mupl Language: Write a mupl interpreter, i.e., a Racket function eval-exp
;; that takes a mupl expression e and either returns the mupl value that e evaluates to under the empty
;; environment or calls Racket’s error if evaluation encounters a run-time mupl type error or unbound
;; mupl variable.
;; A mupl expression is evaluated under an environment (for evaluating variables, as usual). In your
;; interpreter, use a Racket list of Racket pairs to represent this environment (which is initially empty)
;; so that you can use without modification the provided envlookup function.

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

(define (extract-int-value num)
  (if (int? num) (int-num num)
    (error "extract int value from non-number of MUPL")))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (begin
  (displayln e)
  (cond
        ;; All values (including closures) evaluate to themselves. For example, (eval-exp (int 17)) would
        ;; return (int 17), not 17.
        [(int? e) e]
        [(munit? e) e]
        [(closure? e) e]
        ;; A variable evaluates to the value associated with it in the environment.
        [(var? e) 
         (envlookup env (var-string e))]
        ;; An addition evaluates its subexpressions and, assuming they both produce integers, produces the
        ;; integer that is their sum. (Note this case is done for you to get you pointed in the right direction.)
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; An isgreater evaluates its two subexpressions to values v 1 and v 2 respectively. If both values
        ;; are integers, then if v 1 > v 2 the result of the isgreater expression is the mupl value (int 1), else
        ;; the result is the mupl value (int 0).
        [(isgreater? e)
         (let ([v1 (eval-under-env (isgreater-e1 e) env)]
               [v2 (eval-under-env (isgreater-e2 e) env)])
          (begin
          (if (and (int? v1) (int? v2))
              (if (> (extract-int-value v1) (extract-int-value v2)) (int 1) (int 0))
              (error "MUPL isgreater applied to non-number"))))]
        ;; An ifnz evaluates its first expression to a value v 1 . If it is an integer, then if it is not zero, then
        ;; ifnz evaluates its second subexpression, else it evaluates its third subexpression.
        [(ifnz? e)
         (let ([v1 (eval-under-env (ifnz-e1 e) env)]
               [e2 (eval-under-env (ifnz-e2 e) env)]
               [e3 (eval-under-env (ifnz-e3 e) env)])
          (if (= (extract-int-value v1) 0) e2 e3))]

        ;; An mlet expression evaluates its first expression to a value v. Then it evaluates the second
        ;; expression to a value, in an environment extended to map the name in the mlet expression to v.
        [(mlet? e)
          (let ([var (mlet-var e)]
                [exp (eval-under-env (mlet-e e) env)]
                [body (mlet-body e)])
          (eval-under-env body (cons (cons var (eval-under-env exp env)) env)))]

        ;; apair expression evaluates its two subexpressions and produces a (new) pair holding the results.
        [(apair? e)
         (let ([e1 (eval-under-env (apair-e1 e) env)]
               [e2 (eval-under-env (apair-e2 e) env)])
          (apair e1 e2))]

        ;; A first expression evaluates its subexpression. If the result for the subexpression is a pair, then
        ;; the result for the first expression is the e1 field in the pair.
        [(first? e)
          (let ([pair (first-e e)])
           (if (apair? pair)
            (apair-e1 pair)
            (error (format "MUPL first applied to non-pair"))))]

        ;; A second expression evaluates its subexpression. If the result for the subexpression is a pair, then
        ;; the result for the second expression is the e2 field in the pair.
        [(second? e)
          (let ([pair (second-e e)])
           (if (apair? pair)
            (apair-e2 pair)
            (error (format "MUPL second applied to non-pair"))))]

        ;; * An ismunit expression evaluates its subexpression. If the result is an munit expression, then the
        ;; result for the ismunit expression is the mupl value (int 1), else the result is the mupl value (int 0).
        [(ismunit? e)
          (let ([v (eval-under-env e env)])
            (if (munit? v) (int 1) (int 0)))]

        ;; Functions are lexically scoped: A function evaluates to a closure holding the function and the
        ;; current environment.
        [(fun? e)
          (let ([funname (fun-nameopt e)])
           (closure
            (if (null? funname) env (cons (cons funname e) env))
            e))]

        ;; A call evaluates its first and second subexpressions to values. If the first is not a closure, it is
        ;; an error. Else, it evaluates the closure’s function’s body in the closure’s environment extended
        ;; to map the function’s name to the closure (unless the name field is null) and the function’s
        ;; argument-name (i.e., the parameter name) to the result of the second subexpression.
        [(call? e)
          (let ([funexp (call-funexp e)]
                [actual (call-actual e)])
           (if (closure? funexp)
            (let* ([fun (closure-fun funexp)]
                   [formal (fun-formal fun)]
                   [funenv (closure-env funexp)])
             (eval-under-env (fun-body fun) (cons (cons formal actual) e)))
            (error "MUPL can't call a function that isn't a closure")))]

        [#t (error (format "bad MUPL expression: ~v" e))])))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;(displayln (eval-exp (first (apair (int 1) (int 2)))))

        
;; Problem 4

(define (ifmunit e1 e2 e3) "CHANGE")

(define (mlet* bs e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem 5

(define mupl-filter "CHANGE")

(define mupl-all-gt
  (mlet "filter" mupl-filter
        "CHANGE (notice filter is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
