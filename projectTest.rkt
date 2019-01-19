#lang racket

(require "project.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(define tests
  (test-suite
   "Project Tests"

   (check-equal? (eval-exp (plus (num 2) (num 2))) (num 4) "plus simple test")
   (check-equal? (eval-exp (minus (num 2) (num 3))) (num -1) "minus simple test")
   (check-equal? (eval-exp (div (num 2) (num 3))) (num 0) "div simple test")
   (check-equal? (eval-exp (mult (num 2) (num 3))) (num 6) "mult simple test")
   (check-equal? (eval-exp (neg (num 2))) (num -2) "neg simple test")
   (check-equal? (eval-exp (andalso (bool #f) (bool #t))) (bool #f) "andalso simple test")
   (check-equal? (eval-exp (cnd (bool #t) (plus (num 2) (num 3))(num "5"))) (num 5) "cnd simple test")
   (check-equal? (eval-exp (iseq (num 2) (num 2))) (bool #t) "eq1 simple test")
   (check-equal? (eval-exp (iseq (num 2) (num -2))) (bool #f) "eq2 simple test")
   (check-equal? (eval-exp (iseq (bool #t) (bool #t))) (bool #t) "eq3 simple test")
   (check-equal? (eval-exp (iseq (bool #t) (bool #f))) (bool #f) "eq4 simple test")
   (check-equal? (eval-exp (ifnzero (num 0) (bool #f) (bool #t))) (bool #t) "ifnzero simple test")
   (check-equal? (eval-exp (ifleq (num 0) (num 3) (bool #f) (bool #t))) (bool #f) "ifnzero simple test")
   (check-equal? (eval-exp (ifleq (num 3) (num 3) (bool #f) (bool #t))) (bool #f) "ifnzero1 simple test")
   (check-equal? (eval-exp (ifleq (num 4) (num 3) (bool #f) (bool #t))) (bool #t) "ifnzero2 simple test")
   (check-equal? (eval-exp (with "x" (num 4) (plus (var "x") (num 5)))) (num 9) "with simple test")
   (check-equal? (eval-exp (apair (plus (num 3) (num 1)) (plus (num 3) (num 5)))) (apair (num 4) (num 8))  "apair simple test")
   (check-equal? (eval-exp (1st (apair (plus (num 3) (num 1)) (num 8)))) (num 4)  "1st simple test")
   (check-equal? (eval-exp (ismunit (munit))) (bool #t)  "ismunit simple test")
   (check-equal? (eval-exp (apply (lam "a" "b" (ifleq (var "b") (num 6) (plus (var "b") (num 3))
                                                      (apply (var "a") (mult (num 2) (num 3)))))
                                  (num 8))) (num 9) "simple recursive call")
   (check-equal? (eval-exp (ifmunit (munit) (num 8) (num 9))) (num 8) "ifmunit simple test")
   (check-equal? (eval-exp (ifmunit (num 0) (num 8) (num 9))) (num 9) "ifmunit2 simple test")
   (check-equal? (eval-exp (with* (list (cons "x" (num 4)) (cons "y" (var "x"))) (plus (var "x") (var "y")))) (num 8) "with* simple test")
   (check-equal? (eval-exp (ifneq (num 0) (num 0) (num 8) (num 9))) (num 9) "ifneq simple test")
   (check-equal? (eval-exp (ifneq (num 0) (num 1) (num 8) (num 9))) (num 8) "ifneq2 simple test")

   (check-equal? (eval-exp (apply (apply numex-filter (lam null "x" (plus (num 2) (var "x"))))
                                   (apair (num 2) (apair (num 7) (munit)))))  (apair (num 4) (apair (num 9) (munit))) "numex-filter simple test")
   (check-equal? (eval-exp (apply (apply numex-filter (lam null "x" (plus (num 2) (var "x"))))
                                   (apair (num 2) (apair (num -2) (munit)))))  (apair (num 4) (munit)) "numex-filter2 simple test")
   (check-equal? (eval-exp (apply (apply numex-all-gt (num 3))
                                   (apair (num 2) (apair (num 5) (munit)))))  (apair (num 5) (munit)) "numex-all-gt simple test")
   (check-equal? (eval-exp (apply (apply numex-all-gt (num 3))
                                   (apair (num 4) (apair (num 5) (munit)))))  (apair (num 4) (apair (num 5) (munit))) "numex-all-gt simple test")
   
   ; (check-exn (lambda (x) (string=? (exn-message x) "NUMEX addition applied to non-number"))
;               (lambda () (eval-exp (plus (num 2) (bool #t))))
;               "plus bad argument")
; 
;    (check-equal? (mupllist->racketlist
;                   (eval-exp (call (call mupl-all-gt (int 9))
;                                   (racketlist->mupllist 
;                                    (list (int 10) (int 9) (int 15))))))
;                  (list (int 10) (int 15))
;                  "provided combined test using problems 1, 2, and 4")

   ))


(require rackunit/text-ui)
;; runs the test
(run-tests tests)
