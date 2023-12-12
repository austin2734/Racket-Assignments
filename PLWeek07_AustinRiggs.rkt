;PLWeek07
;Austin Riggs
;10/4/23


#lang racket

(define (myRemove myList target)
  (if (= target (car myList))
      (cdr myList)
      (cons (car myList) (myRemove (cdr myList) target))))

(define (findMax myList maxElement)
   (cond
     [(equal? myList '()) maxElement]
     [(> (car myList) maxElement) (findMax (cdr myList) (car myList))]
     [(findMax (cdr myList) maxElement)]))


(define (myBadSort myList newList )
  (if (equal? myList '())
      newList
  (let ([max (findMax (cdr myList)(car myList))])
       (myBadSort (myRemove myList max)(append (list max) newList)))))


(myBadSort '( 20 13 74 5 12 9 22 95 22 6 101 72 3 53 33 21 96) '())