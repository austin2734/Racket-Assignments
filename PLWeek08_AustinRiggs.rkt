;PLWeek08
;Austin Riggs
;10/14/23

#lang racket

(define (tree-insert num tree)
  (cond
     [(empty? tree) (list num)]
     [(empty? (cdr tree)) (tree-insert num (list(car tree) '(() ())))]
     [(<=  num (car tree)) (list (car tree) (list (tree-insert num (caadr tree)) (cadadr tree)))]
     [(>= num (car tree)) (list (car tree) (list (caadr tree)(tree-insert num (cadadr tree))))]))



(tree-insert 8 '())
(tree-insert 12 '(8))
(tree-insert 3 '(8))
(tree-insert 12 '(8 ((3) ())))
(tree-insert 4 '(8 ((3) (12))))


(define (list-to-tree numList tree)
  (if (empty? numList) tree
   (list-to-tree (cdr numList) (tree-insert (car numList) tree))))


(list-to-tree '(22 25 7 16 8 34 67 7 32 17 8 4  5 3) '())

; Two Base cases. If tree is empty, an empty list is returned. In my other base case, when cdr of tree is empty, car of list is taken and
; converted into a list. This is done so that the append method may be used in the third conditon of my function.
(define (tree-to-list tree)
  (cond
    [(empty? tree) '()]
    [(empty? (cdr tree)) (list (car tree))]
    [(append(tree-to-list(caadr tree))(list(car tree))(tree-to-list (cadadr tree)))]))


(tree-to-list (list-to-tree '(22 25 7 16 8 34 67 7 32 17 8 4  5 3) '()) )
