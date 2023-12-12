;PLWeek02
;Austin Riggs
;8/28/23

#lang racket
(define (equation x)
  (-(+ 30(* 2 x))(* 3(expt x 2))))

(define (resultWithMessage x)
  (let ([result (equation x)])
    (cond [(> result 10)
           (printf "The result is greater than 10, result = ~a\n" result)]
          [(< result -10)
           (printf "The result is less than -10, result = ~a\n" result)]
          [else
           (printf "The result is between -10 and 10, result = ~a\n" result)])))


(printf "Test cases for equation fuction:\n")
(equation -1.5)
(equation 3)
(equation 20)

(printf "Test cases for resultWithMessage function:\n")
(resultWithMessage 3.1)
(resultWithMessage 3000)
(resultWithMessage .6578)
