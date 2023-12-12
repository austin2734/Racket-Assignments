;PLWeek05
;Austin Riggs
;9/21/23

#lang racket


(define (poly x)
  (-(-(* 9(* x x))(* 3 x))25))


(define (poly2 x)
  (-(-(* 11(* x x))(* 2 x))50))


(define (root-iter func x1 x2 tol)
  (let* ([y1 (func x1)]
         [y2 (func x2)]
         [x3 (- x2 (/ (* (- x2 x1) y2) (- y2 y1)))]
         [y3 (func x3)])
    (cond
     [(< (abs y3) tol) x3]
     [(root-iter func x2 x3 tol)])))


(display "Root of first polynomial: " )
(exact->inexact(root-iter poly 0 4 .00001))
(display  "Result of plugging in result to first polynomial: " )
(poly 1.8416459368502338)

(newline)

(display "Root of second polynomial: " )
(exact->inexact(root-iter poly2 -5.0 1.0 .00001))
(display "Result of plugging in result to second polynomial: " )
(poly2 2.224853562600938)
















































































































































           
           











           
           
           










           