;PLWeek04
;Austin Riggs
;9/13/23


#lang racket

(define (findSign i)
  (modulo i 2))


(define (mercator iteration x)
  (if (= 0 iteration )
      0
      (if (= 0 (findSign iteration))
          (+ (* -1(/(expt x iteration) iteration))
             (mercator (- iteration 1) x))
          (+ (/(expt x iteration) iteration)
             (mercator (- iteration 1) x)))))




(define (mercator2 iteration x)
  (if (= 0 iteration)
      0
      (if (lambda(x)(= 0 (modulo x 2)))
          (+ (* -1(/(expt x iteration) iteration))
             (mercator (- iteration 1) x))
          (+ (/(expt x iteration) iteration)
             (mercator (- iteration 1) x)))))



(mercator 100 .5)
(mercator 100 -.5)

(mercator2 100 .5)
(mercator2 100 -.5)
