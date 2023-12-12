;PLWeek03
;Austin Riggs
;9/5/23

#lang racket


(define (basel x)
  (if (< x 2)
      1.0
      (+ (expt (/ 1 x) 4)(basel(- x 1)))))


(exact->inexact(basel 100))