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
      (munit)
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

(define (display-list items)
  (if (null? items)
    #f
    (begin
      (displayln (car items))
      (display-list (cdr items)))))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond
        ;; All values (including closures) evaluate to themselves. For example, (eval-exp (int 17)) would
        ;; return (int 17), not 17.
        [(int? e) e]
        [(munit? e) e]
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
          (if (and (int? v1) (int? v2))
              (if (> (extract-int-value v1) (extract-int-value v2)) (int 1) (int 0))
              (error "MUPL isgreater applied to non-number")))]

        ;; An ifnz evaluates its first expression to a value v 1 . If it is an integer, then if it is not zero, then
        ;; ifnz evaluates its second subexpression, else it evaluates its third subexpression.
        [(ifnz? e)
         (let ([v1 (eval-under-env (ifnz-e1 e) env)])
          (if (= (extract-int-value v1) 0)
           (eval-under-env (ifnz-e3 e) env)
           (eval-under-env (ifnz-e2 e) env)))]

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
          (let ([pair (eval-under-env (first-e e) env)])
           (if (apair? pair)
            (apair-e1 pair)
            (error (format "MUPL first applied to non-pair"))))]

        ;; A second expression evaluates its subexpression. If the result for the subexpression is a pair, then
        ;; the result for the second expression is the e2 field in the pair.
        [(second? e)
          (let ([pair (eval-under-env (second-e e) env)])
           (if (apair? pair)
            (apair-e2 pair)
            (error (format "MUPL second applied to non-pair"))))]

        ;; * An ismunit expression evaluates its subexpression. If the result is an munit expression, then the
        ;; result for the ismunit expression is the mupl value (int 1), else the result is the mupl value (int 0).
        [(ismunit? e)
          (let ([v (eval-under-env (ismunit-e e) env)])
            (if (munit? v) (int 1) (int 0)))]

        ;; Functions are lexically scoped: A function evaluates to a closure holding the function and the
        ;; current environment.
        [(fun? e) (closure env e)]

        ;; A call evaluates its first and second subexpressions to values. If the first is not a closure, it is
        ;; an error. Else, it evaluates the closure’s function’s body in the closure’s environment extended
        ;; to map the function’s name to the closure (unless the name field is null) and the function’s
        ;; argument-name (i.e., the parameter name) to the result of the second subexpression.
        [(call? e)
          (let ([funexp (eval-under-env (call-funexp e) env)]
                [actual (eval-under-env (call-actual e) env)])
           (if (closure? funexp)
            (let* ([fun (closure-fun funexp)]
                   [funname (fun-nameopt fun)]
                   [formal (fun-formal fun)]
                   [fun_env (closure-env funexp)]
                   [cur_env (if funname (cons (cons funname funexp) fun_env) fun_env)])
             (eval-under-env (fun-body fun) (cons (cons formal actual) cur_env)))
            (error "MUPL can't call a function that isn't a closure")))]

        [(closure? e) e]

        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))


;; Problem 4
;; Expanding the Language: mupl is a small language, but we can write Racket functions that act like
;; mupl macros so that users of these functions feel like mupl is larger. The Racket functions produce
;; mupl expressions that could then be put inside larger mupl expressions or passed to eval-exp. In
;; implementing these Racket functions, do not use closure (which is used only internally in eval-exp).
;; Also do not use eval-exp (we are creating a program, not running it).


;; (a) Write a Racket function ifmunit that takes three mupl expressions e1 , e2 , and e3 . It returns a
;; mupl expression that when run evaluates e1 and if the result is mupl’s munit then it evaluates
;; e2 and that is the result, else it evaluates e3 and that is the result. Sample solution: 1 line.
(define (ifmunit e1 e2 e3) (ifnz (ismunit e1) e2 e3))

;; (b) Write a Racket function mlet* that takes a Racket list of Racket pairs ’((s1 . e1) ... (si . ei)
;; ... (sn . en)) and a final mupl expression e_n+1 . In each pair, assume si is a Racket string and
;; ei is a mupl expression. mlet* returns a mupl expression whose value is e_n+1 evaluated in an
;; environment where each si is a variable bound to the result of evaluating the corresponding ei
;; for 1 ≤ i ≤ n. The bindings are done sequentially, so that each ei is evaluated in an environment
;; where s1 through s_i−1 have been previously bound to the values e1 through e_i−1 .
(define (mlet* bs e2)
  (if (null? bs)
   e2
   (let ([key (caar bs)]
         [value (cdar bs)])
    (mlet key value (mlet* (cdr bs) e2)))))

;; (c) Write a Racket function ifeq that takes four mupl expressions e1 , e2 , e3 , and e4 and returns
;; a mupl expression that acts like ifnz except e3 is evaluated if and only if e1 and e2 are equal
;; integers. (An error occurs if the result of e1 or e2 is not an integer.) Assume none of the arguments
;; to ifeq use the mupl variables _x or _y. Use this assumption so that when an expression returned
;; from ifeq is evaluated, e1 and e2 are evaluated exactly once each.
(define (ifeq e1 e2 e3 e4) 
  (mlet* (list (cons "_x" e1)
               (cons "_y" e2)
               (cons "_z" (add (isgreater (var "_x") (var "_y")) (isgreater (var "_y") (var "_x")))))
   (ifnz (var "_z") e4 e3)))

;; Problem 5
;; Using the Language: We can write mupl expressions directly in Racket using the constructors for
;; the structs and (for convenience) the functions we wrote in the previous problem.

;; (a) Bind to the Racket variable mupl-filter a mupl function that acts like filter (as we used in ML).
;; Your function should be curried: it should take a mupl function and return a mupl function that
;; takes a mupl list and applies the function to every element of the list returning a new mupl list
;; with all the elements for which the function returns a number other than zero (causing an error
;; if the function returns a non-number). Recall a mupl list is munit or a pair where the second
;; component is a mupl list.

(define mupl-filter
  (fun null "func"
    (fun "do_filter" "items" 
      (ifmunit (var "items")
       (munit)
       (mlet* (list [cons "tl" (call (var "do_filter") (second (var "items")))]
                    [cons "hd" (first (var "items"))])
          (ifnz (call (var "func") (var "hd"))
            (apair (var "hd") (var "tl"))
            (var "tl")))))))

;; (b) Bind to the Racket variable mupl-all-gt a mupl function that takes an mupl integer i and
;; returns a mupl function that takes a mupl list of mupl integers and returns a new mupl list of
;; mupl integers containing the elements of the input list (in order) that are greater than i. Use
;; mupl-filter (a use of mlet is given to you to make this easier).
(define mupl-all-gt
  (mlet "filter" mupl-filter
    (fun null "i"
      (call (var "filter")
            (fun null "item" (isgreater (var "item") (var "i")))))))


;; Challenge Problem
;; Write a second version of eval-exp (bound to eval-exp-c) that builds closures
;; with smaller environments: When building a closure, it uses an environment that is like the current
;; environment but holds only variables that are free variables in the function part of the closure. (A free
;; variable is a variable that appears in the function without being under some shadowing binding for the
;; same variable.)
;; Avoid computing a function’s free variables more than once by writing a function compute-free-vars
;; that takes an expression and returns a different expression that uses fun-challenge everywhere in
;; place of fun. The new struct fun-challenge (provided to you; do not change it) has a field freevars
;; to store exactly the set of free variables for the function. Store this set as a Racket set of Racket strings.
;; (Sets are predefined in Racket’s standard library; consult the documentation for useful functions such
;; as set, set-add, set-member?, set-remove, set-union, and any other functions you wish.)
;; You must have a top-level function compute-free-vars that works as just described — storing the
;; free variables of each function in the freevars field — so the grader can test it directly. Then write a
;; new “main part” of the interpreter that expects the sort of mupl expression that compute-free-vars
;; returns. The case for function definitions is the interesting one.

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) 
  (letrec ([helper (lambda (in-e fvars)
                     (cond [(var? in-e) (set-add fvars (var-string in-e))]
                           [(add? in-e) (set-union (helper (add-e1 in-e) fvars) (helper (add-e2 in-e) fvars))]
                           [(isgreater? in-e) (set-union (helper (isgreater-e1 in-e) fvars)
                                                         (helper (isgreater-e2 in-e) fvars))]
                           [(ifnz? in-e) (set-union (helper (ifnz-e1 in-e) fvars)
                                                    (helper (ifnz-e2 in-e) fvars)
                                                    (helper (ifnz-e3 in-e) fvars))]
                           [(apair? in-e) (set-union (helper (apair-e1 in-e) fvars)
                                                     (helper (apair-e2 in-e) fvars))]
                           [(first? in-e) (helper (first-e in-e) fvars)]
                           [(second? in-e) (helper (second-e in-e) fvars)]
                           [(ismunit? in-e) (helper (ismunit-e in-e) fvars)]
                           [(mlet? in-e) (set-remove (helper (mlet-body in-e) fvars) (mlet-var in-e))]
                           [(call? in-e) (set-union (helper (call-funexp in-e) fvars)
                                                    (helper (call-actual in-e) fvars))]
                           [(fun? in-e) (set-subtract (helper (fun-body in-e) fvars)
                                                      (if (fun-nameopt in-e)
                                                          (set (fun-nameopt in-e) (fun-formal in-e))
                                                          (set (fun-formal in-e))))]
                           [#t fvars]
                           ))])
    (cond [(add? e) (add (compute-free-vars (add-e1 e)) (compute-free-vars (add-e2 e)))]
          [(isgreater? e) (isgreater (compute-free-vars (isgreater-e1 e))
                                     (compute-free-vars (isgreater-e2 e)))]
          [(ifnz? e) (ifnz (compute-free-vars (ifnz-e1 e))
                           (compute-free-vars (ifnz-e2 e))
                           (compute-free-vars (ifnz-e3 e)))]
          [(apair? e) (apair (compute-free-vars (apair-e1 e))
                             (compute-free-vars (apair-e2 e)))]
          [(first? e) (first (compute-free-vars (first-e e)))]
          [(second? e) (second (compute-free-vars (second-e e)))]
          [(ismunit? e) (ismunit (compute-free-vars (ismunit-e e)))]
          [(mlet? e) (mlet (mlet-var e) (compute-free-vars (mlet-e e)) (compute-free-vars (mlet-body e)))]
          [(call? e) (call (compute-free-vars (call-funexp e))
                           (compute-free-vars (call-actual e)))]
          [(fun? e)
           (fun-challenge (fun-nameopt e) (fun-formal e) (compute-free-vars (fun-body e)) (helper e (set)))]
          [#t e])))

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) 
  (cond
        ;; All values (including closures) evaluate to themselves. For example, (eval-exp (int 17)) would
        ;; return (int 17), not 17.
        [(int? e) e]
        [(munit? e) e]
        ;; A variable evaluates to the value associated with it in the environment.
        [(var? e) 
         (envlookup env (var-string e))]
        ;; An addition evaluates its subexpressions and, assuming they both produce integers, produces the
        ;; integer that is their sum. (Note this case is done for you to get you pointed in the right direction.)
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]

        ;; An isgreater evaluates its two subexpressions to values v 1 and v 2 respectively. If both values
        ;; are integers, then if v 1 > v 2 the result of the isgreater expression is the mupl value (int 1), else
        ;; the result is the mupl value (int 0).
        [(isgreater? e)
         (let ([v1 (eval-under-env-c (isgreater-e1 e) env)]
               [v2 (eval-under-env-c (isgreater-e2 e) env)])
          (if (and (int? v1) (int? v2))
              (if (> (extract-int-value v1) (extract-int-value v2)) (int 1) (int 0))
              (error "MUPL isgreater applied to non-number")))]

        ;; An ifnz evaluates its first expression to a value v 1 . If it is an integer, then if it is not zero, then
        ;; ifnz evaluates its second subexpression, else it evaluates its third subexpression.
        [(ifnz? e)
         (let ([v1 (eval-under-env-c (ifnz-e1 e) env)])
          (if (= (extract-int-value v1) 0)
           (eval-under-env-c (ifnz-e3 e) env)
           (eval-under-env-c (ifnz-e2 e) env)))]

        ;; An mlet expression evaluates its first expression to a value v. Then it evaluates the second
        ;; expression to a value, in an environment extended to map the name in the mlet expression to v.
        [(mlet? e)
          (let ([var (mlet-var e)]
                [exp (eval-under-env-c (mlet-e e) env)]
                [body (mlet-body e)])
          (eval-under-env-c body (cons (cons var (eval-under-env-c exp env)) env)))]

        ;; apair expression evaluates its two subexpressions and produces a (new) pair holding the results.
        [(apair? e)
         (let ([e1 (eval-under-env-c (apair-e1 e) env)]
               [e2 (eval-under-env-c (apair-e2 e) env)])
          (apair e1 e2))]

        ;; A first expression evaluates its subexpression. If the result for the subexpression is a pair, then
        ;; the result for the first expression is the e1 field in the pair.
        [(first? e)
          (let ([pair (eval-under-env-c (first-e e) env)])
           (if (apair? pair)
            (apair-e1 pair)
            (error (format "MUPL first applied to non-pair"))))]

        ;; A second expression evaluates its subexpression. If the result for the subexpression is a pair, then
        ;; the result for the second expression is the e2 field in the pair.
        [(second? e)
          (let ([pair (eval-under-env-c (second-e e) env)])
           (if (apair? pair)
            (apair-e2 pair)
            (error (format "MUPL second applied to non-pair"))))]

        ;; * An ismunit expression evaluates its subexpression. If the result is an munit expression, then the
        ;; result for the ismunit expression is the mupl value (int 1), else the result is the mupl value (int 0).
        [(ismunit? e)
          (let ([v (eval-under-env-c (ismunit-e e) env)])
            (if (munit? v) (int 1) (int 0)))]

        ;; Functions are lexically scoped: A function evaluates to a closure holding the function and the
        ;; current environment.
        [(fun-challenge? e)
          (closure
            (let ([freevars (fun-challenge-freevars e)])
              (filter (lambda (x) (set-member? freevars (car x))) env))
            e)]

        ;; A call evaluates its first and second subexpressions to values. If the first is not a closure, it is
        ;; an error. Else, it evaluates the closure’s function’s body in the closure’s environment extended
        ;; to map the function’s name to the closure (unless the name field is null) and the function’s
        ;; argument-name (i.e., the parameter name) to the result of the second subexpression.
        [(call? e)
          (let ([funexp (eval-under-env-c (call-funexp e) env)]
                [actual (eval-under-env-c (call-actual e) env)])
           (if (closure? funexp)
            (let* ([fun (closure-fun funexp)]
                   [funname (fun-challenge-nameopt fun)]
                   [formal (fun-challenge-formal fun)]
                   [fun_env (closure-env funexp)]
                   [cur_env (if funname (cons (cons funname funexp) fun_env) fun_env)])
             (eval-under-env-c (fun-challenge-body fun) (cons (cons formal actual) cur_env)))
            (error "MUPL can't call a function that isn't a closure")))]

        [(closure? e) e]

        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
