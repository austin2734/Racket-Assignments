;PLWeek06
;Austin Riggs
;9/26/23

#lang racket


(define (listLength list count)
  (if (equal? list '())
      count
      (listLength (cdr list)(+ 1 count))))


(listLength '(1 2 3 4) 0)
(listLength '(1 (2 3) (4 (5 6))) 0)


(define (deepListLength list count)
  (cond
     [(equal? list '()) count]
     [(list? (car list)) (deepListLength (cdr list) (+ count (deepListLength(car list) 0)))]
     [(deepListLength (cdr list)(+ 1 count))]))

(deepListLength '(1 2 3 4) 0)
(deepListLength '(1 (2 3) (4 (5 6))) 0)