;; PL Project - Fall 2018
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)  ;; a constant number, e.g., (num 17)
(struct bool (boolean) #:transparent) ;; boolean
(struct plus  (e1 e2)  #:transparent)  ;; add two expressions
(struct minus  (e1 e2)  #:transparent)  ;; sub two expressions
(struct mult  (e1 e2)  #:transparent)  ;; mul two expressions
(struct div  (e1 e2)  #:transparent)  ;; div two expressions
(struct neg  (e1)  #:transparent)
(struct andalso  (e1 e2)  #:transparent)  
(struct orelse  (e1 e2)  #:transparent) 
(struct cnd  (e1 e2 e3)  #:transparent) 
(struct iseq  (e1 e2)  #:transparent) 
(struct ifnzero  (e1 e2 e3)  #:transparent) 
(struct ifleq  (e1 e2 e3 e4)  #:transparent) 


(struct lam  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct apply (funexp actual)       #:transparent) ;; function application

(struct with (s e1 e2) #:transparent)

(struct apair (e1 e2) #:transparent)
(struct 1st (e1) #:transparent)
(struct 2nd (e1) #:transparent)



(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then true else false

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent)


(define (isNumex x) (
  cond
    ((bool? x) (if (boolean? (bool-boolean x)) #t #f) )
    ((var? x) (if (string? (var-string x)) #t #f))
    ((num? x) (if (integer? (num-int x)) #t #f) )
    ((apair? x) (if (and (isNumex (1st x)) (isNumex (2nd x))) #t #f) )
    ((munit? x) #t)
    (#t #f)
    ;;;TODO ((closure? x) (if (boolean? (bool-boolean x)) (x) (error "value of bool must be boolean")) )
))

;; Problem 1

(define (racketlist->numexlist xs) (
  cond
    ((null? xs) (munit))
    ((munit? (car xs)) (apair munit (racketlist->numexlist (cdr xs))))
    ((list? (car xs)) (apair (racketlist->numexlist (car xs)) (racketlist->numexlist (cdr xs))))
    ((isNumex (car xs)) (apair (car xs) (racketlist->numexlist(cdr xs)) ))
    ((integer? (car xs)) (apair (num (car xs)) (racketlist->numexlist (cdr xs))))
    ;;;TODO (string? (car xs) (apair (var (car xs)) (racketlist->numexlist (cdr xs))))
    (boolean? (car xs) (apair (bool (car xs)) (racketlist->numexlist (cdr xs))))
    (#t (error "invalid input"))
    
))
(define (numexlist->racketlist xs) (
    cond
        (
            (apair? xs) (
            cond
                ;;; ((munit? xs) null)
                ((num? (1st xs)) (cons (num-int (1st xs)) (numexlist->racketlist (2nd xs))))
                ((bool? (1st xs)) (cons (bool-boolean (1st xs)) (numexlist->racketlist (2nd xs))))
                ((apair? (1st xs)) (cons (numexlist->racketlist (1st xs)) (numexlist->racketlist (2nd xs))))
                (#t (error "Invalid numex item"))
            )
        )
        ((munit? xs) null)
        (#t  (error "argument must be a numex apair"))
))
; (numexlist->racketlist (apair (num 2) (apair (bool #t) (munit))))
; (numexlist->racketlist (apair (num 1) (apair (num 2) (apair (num 3) (apair (apair (bool #t) (apair (num 1) (apair (num 2) (munit)))) (apair (num 5) (munit)))))))
; (racketlist->numexlist (list 1 2 (num 3) 3 4))
;   (racketlist->numexlist (list 1 2 3 (list  #t 1 2) 5))

 ;; Problem 2
 
 ;; lookup a variable in an environment
 ;; Complete this function
 (define (envlookup env str)
   (cond [(null? env) (error "unbound variable during evaluation" str)]
         [(eq? (car (car env)) str) (cdr (car env))]
         [#t (envlookup (cdr env) str)]
   )
 )
 
 ;; Complete more cases for other kinds of NUMEX expressions.
 ;; We will test eval-under-env by calling it directly even though
 ;; "in real life" it would be a helper function of eval-exp.
 (define (eval-under-env e env)
   (cond [(var? e) 
          (envlookup env (var-string e))]
         [(num? e) (if (isNumex e) e (eval-under-env (num-int e) env))]
         [(bool? e) (if (isNumex e) e (eval-under-env (num-int e) env))]
         [(closure? e) (if (isNumex e) e (eval-under-env (num-int e) env))]
         ; wrong! [(apair? e) (if (isNumex e) e (error "numex apair :("))]
         [(munit? e) (if (isNumex e) e (eval-under-env (num-int e) env))]
         
         [(plus? e) 
          (let ([v1 (eval-under-env (plus-e1 e) env)]
                [v2 (eval-under-env (plus-e2 e) env)])
            (if (and (num? v1)
                     (num? v2))
                (num (+ (num-int v1) 
                        (num-int v2)))
                (error "NUMEX addition applied to non-number")))]
         [(minus? e) 
          (let ([v1 (eval-under-env (minus-e1 e) env)]
                [v2 (eval-under-env (minus-e2 e) env)])
            (if (and (num? v1)
                     (num? v2))
                (num (- (num-int v1) 
                        (num-int v2)))
                (error "NUMEX substraction applied to non-number")))]
         [(mult? e) 
          (let ([v1 (eval-under-env (mult-e1 e) env)]
                [v2 (eval-under-env (mult-e2 e) env)])
            (if (and (num? v1)
                     (num? v2))
                (num (* (num-int v1) 
                        (num-int v2)))
                (error "NUMEX multiplication applied to non-number")))]
         [(div? e) 
          (let ([v1 (eval-under-env (div-e1 e) env)]
                [v2 (eval-under-env (div-e2 e) env)])
            (if (and (num? v1)
                     (num? v2))
                [if (not (= (num-int v2) 0))
                    (num (exact-floor (/ (num-int v1) (num-int v2))))
                    (error "NUMEX division by zero")]
                (error "NUMEX division applied to non-number")))]
         [(neg? e)
          (let ([v1 (eval-under-env (neg-e1 e) env)])
            (cond
              [(num? v1) (num (- (num-int v1)))]
              [(bool? v1) (bool (not (bool-boolean v1)))]
               [#t (error "NUMEX negation applied to non-number or non-boolean")]
               ))
         ]
         [(andalso? e)
          (let ([v1 (eval-under-env (andalso-e1 e) env)]
                [v2 (eval-under-env (andalso-e2 e) env)])
             (if (and (bool? v1)
                     (bool? v2))
                 (bool (and (bool-boolean v1) (bool-boolean v2)))
                 (error "NUMEX and applied to non-booleab")))
          ]
         [(orelse? e)
          (let ([v1 (eval-under-env (andalso-e1 e) env)]
                [v2 (eval-under-env (andalso-e2 e) env)])
             (if (and (bool? v1)
                     (bool? v2))
                 (bool (or (bool-boolean v1) (bool-boolean v2)))
                 (error "NUMEX or applied to non-booleab")))
          ]
         [(cnd? e)
          (let ([v1 (eval-under-env (cnd-e1 e) env)])
             (if (bool? v1)
                 (if (bool-boolean  v1)
                     (eval-under-env (cnd-e2 e) env)
                     (eval-under-env (cnd-e3 e) env))
                 (error "NUMEX cnd applied to non-booleab")))
          ]
         [(iseq? e)
          (let ([v1 (eval-under-env (iseq-e1 e) env)]
                [v2 (eval-under-env (iseq-e2 e) env)])
            (cond
              [(and (bool? v1) (bool? v2)) (bool (eq? (bool-boolean v1) (bool-boolean v2)))]
              [(and (num? v1) (num? v2)) (bool (eq? (num-int v1) (num-int v2)))]
              [#t   (error "NUMEX eq applied to non-booleab or non-num")]
            ))
          ]
         [(ifnzero? e)
          (let ([v1 (eval-under-env (ifnzero-e1 e) env)])
             (if (num? v1)
                 (if (zero? (num-int v1))
                     (eval-under-env (ifnzero-e3 e) env)
                     (eval-under-env (ifnzero-e2 e) env))
                 (error "NUMEX ifnzero applied to non-num")))
          ]
         [(ifleq? e)
          (let ([v1 (eval-under-env (ifleq-e1 e) env)]
                [v2 (eval-under-env (ifleq-e2 e) env)])
             (if (and (num? v2) (num? v1))
                 (if (<= (num-int v1) (num-int v2))
                     (eval-under-env (ifleq-e3 e) env)
                     (eval-under-env (ifleq-e4 e) env))
                 (error "NUMEX ifleq applied to non-num")))]
         [(with? e)
          (let ([v1 (eval-under-env (with-e1 e) env)]
                [s (with-s e)])
             (if (string? s)
                 (eval-under-env (with-e2 e) (append env (list (cons s v1))))
                 (error "NUMEX with expected string")))
          ]
         [(apair? e)
          (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
            (apair v1 v2)
            )
          ]
         [(1st? e)
          (let ([v1 (eval-under-env (1st-e1 e) env)])
            (if (apair? v1)
                (apair-e1 v1)
                (error "NUMEX 1st expected apair"))
            )
          ]
         [(2nd? e)
          (let ([v1 (eval-under-env (2nd-e1 e) env)])
            (if (apair? v1)
                (apair-e2 v1)
                (error "NUMEX 2st expected apair"))
            )
          ]
         [(ismunit? e)
          (let ([v1 (eval-under-env (ismunit-e e) env)])
            (if (munit? v1)
                (bool #t)
                (bool #f)))
          ]
         [(lam? e)
         (if (and (or (string? (lam-nameopt e)) (null? (lam-nameopt e))) (string? (lam-formal e)))
             (closure env e)
             (error "NUMEX function name and parameter name must be string"))]
        [(apply? e)
         (let ([v (eval-under-env (apply-actual e) env)]
               [clsr (eval-under-env (apply-funexp e) env)])
           (if (closure? clsr)
               (let ([clsrFun (closure-f clsr)])
                 (if (null? (lam-nameopt clsrFun))
                     (eval-under-env (lam-body clsrFun) (cons (cons (lam-formal clsrFun) v) (closure-env clsr)))
                     (eval-under-env (lam-body clsrFun) (cons (cons (lam-nameopt clsrFun) clsr) (cons (cons (lam-formal clsrFun) v) (closure-env clsr))))))
               (error "NUMEX call applied to non-function" e)))]
         ;; CHANGE add more cases here
         
         [#t (error (format "bad NUMEX expression: ~v" e))]))
 
 ;; Do NOT change
 (define (eval-exp e)
   (eval-under-env e null))
;; Problem 3

(define (ifmunit e1 e2 e3) (cnd (ismunit e1) e2 e3))
 
 (define (with* bs e2) (
                        cond
                         [(null? bs) e2]
                         [#t (with (caar bs) (cdar bs) (with* (cdr bs) e2))]
                         ))
 
 (define (ifneq e1 e2 e3 e4) (cnd (iseq e1 e2) e4 e3))
 
 ;; Problem 4
 
 (define numex-filter (
                       lam null "fun"
                           (lam "map" "xs"
                                (cnd (ismunit (var "xs")) (munit)
                                     (with "res" (apply (var "fun") (1st (var "xs")))
                                                                        (ifnzero (var "res")
                                                                             (apair (var "res") (apply (var "map") (2nd (var "xs"))))
                                                                             (apply (var "map") (2nd (var "xs"))))
                                                                             ))
                           )
                       ))
 
 (define numex-all-gt
   (with "filter" numex-filter
         (lam null "i"
              (lam "gt" "xs" (apply (apply (var "filter") (lam null "in"
                                                        (ifleq (var "in") (var "i") (num 0) (num (var "in")) )
                                                    )) (var "xs")))
              )))
 
 ;; Challenge Problem
 
 (struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function
 
 ;; We will test this function directly, so it must do
 ;; as described in the assignment
 (define (compute-free-vars e) (compute-free-vars-fun e (set) (set)))

(define (compute-free-vars-fun e bound unbound) (cond
                                 [(num? e) unbound]
                                 [(bool? e) unbound]
                                 [(var? e) (if (set-member? bound (var-string e)) (unbound) (set-add unbound (var-string e)))]

                                 [(closure? e) unbound]
         
                                 [(munit? e) unbound]
                                 
                                 [(plus? e) 
                                  (let ([v1 (compute-free-vars-fun (plus-e1 e) bound unbound)]
                                        [v2 (compute-free-vars-fun (plus-e2 e) bound unbound)])
                                    (set-union v1 v2)
                                    )]
                                 [(minus? e)
                                  (let ([v1 (compute-free-vars-fun (minus-e1 e) bound unbound)]
                                        [v2 (compute-free-vars-fun (minus-e2 e) bound unbound)])
                                    (set-union v1 v2)
                                    )
                                  ]
                                 [(mult? e)
                                  (let ([v1 (compute-free-vars-fun (mult-e1 e) bound unbound)]
                                        [v2 (compute-free-vars-fun (mult-e2 e) bound unbound)])
                                    (set-union v1 v2)
                                    )
                                  ]
                                 [(div? e)
                                  (let ([v1 (compute-free-vars-fun (div-e1 e) bound unbound)]
                                        [v2 (compute-free-vars-fun (div-e2 e) bound unbound)])
                                    (set-union v1 v2)
                                    )
                                  ]
                                 [(neg? e)
                                  (let ([v1 (compute-free-vars-fun (plus-e1 e) bound unbound)])
                                    (v1)
                                    )
                                  ]
                                 [(andalso? e)
                                  (let ([v1 (compute-free-vars-fun (andalso-e1 e) bound unbound)]
                                        [v2 (compute-free-vars-fun (andalso-e2 e) bound unbound)])
                                    (set-union v1 v2)
                                    )
                                  ]
                                 [(orelse? e)
                                  (let ([v1 (compute-free-vars-fun (orelse-e1 e) bound unbound)]
                                        [v2 (compute-free-vars-fun (orelse-e2 e) bound unbound)])
                                    (set-union v1 v2)
                                    )
                                  ]
                                 [(cnd? e)
                                  (let ([v1 (compute-free-vars-fun (cnd-e1 e) bound unbound)]
                                        [v2 (compute-free-vars-fun (cnd-e2 e) bound unbound)]
                                        [v3 (compute-free-vars-fun (cnd-e3 e) bound unbound)])
                                    (set-union v1 v2 v1)
                                    )
                                  ]
                                 [(iseq? e)
                                  (let ([v1 (compute-free-vars-fun (cnd-e1 e) bound unbound)]
                                        [v2 (compute-free-vars-fun (cnd-e2 e) bound unbound)]
                                        [v3 (compute-free-vars-fun (cnd-e3 e) bound unbound)])
                                    (set-union v1 v2 v1)
                                    )
                                  ]
                                 [(ifnzero? e)
                                  ]
                                 [(ifleq? e)
                                  ]
                                 [(with? e)
                                  (let ([v1 (set-add unbound (with-s e))]) (set-union v1 (compute-free-vars-fun (with-e2 e) bound v1)))
                                  ]
                                 [(apair? e)
                                  ]
                                 [(1st? e)
                                  ]
                                 [(2nd? e)
                                  ]
                                 [(ismunit? e)
                                  ]
                                 [(lam? e)
                                  ]
                                 [(apply? e)
                                  ]
                                 ;; CHANGE add more cases here
                                 
                                 [#t (error (format "bad NUMEX expression: ~v" e))]))
         (compute-free-vars-fun (with "x" (num 1) (plus (var "x") (num 2))) (set) (set))                              
 ;; Do NOT share code with eval-under-env because that will make grading
 ;; more difficult, so copy most of your interpreter here and make minor changes
 (define (eval-under-env-c e env) "CHANGE")
 
 ;; Do NOT change this
 (define (eval-exp-c e)
   (eval-under-env-c (compute-free-vars e) null))
 
