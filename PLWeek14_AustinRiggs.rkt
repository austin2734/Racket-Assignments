;PLWeek14
;Austin Riggs
;11/21/23

#lang racket

; import libraries
(require racket/draw)
(require colors)
(require racket/trace)
(require racket/math)


; keeps count of total polygons drawn
(define polyCount 0)

; height and width of background
(define imageWidth 2048)
(define imageHeight 1152)

; bitmap and draw context
(define myTarget (make-bitmap imageWidth imageHeight)) ; A bitmap
(define dc (new bitmap-dc% [bitmap myTarget])) ; a drawing context

; used to scale to wider world screen
(define xScale .5)
(define yScale .5)
(define xTrans 0)
(define yTrans 100)

;background image
(send dc set-brush "White Smoke" 'solid)
(send dc draw-rectangle
      0 0
      2048 1152)
(send dc set-brush "Black" 'solid)

(define smallestPoly 280)

; draws polygons to screen
(define (drawToScreen polygon)
 (send polygon scale xScale yScale)
 (send polygon translate xTrans yTrans)
 (send dc draw-path polygon)
 (send polygon translate (- xTrans) (- yTrans))
 (send polygon scale (/ 1 xScale) (/ 1 yScale)))


; base polygon used to draw trees
(define treePolygon (new dc-path%))
(send treePolygon move-to 0 0)
(send treePolygon line-to 250 0)
(send treePolygon line-to 250 30)
(send treePolygon line-to 0 30)
(send treePolygon line-to 0 0)
(send treePolygon close)

; duplicates input polygon
(define (duplicatePolygon oldPolygon)
  (let ((newPolygon (new dc-path%)))
  (send newPolygon append oldPolygon)
   newPolygon))

; calculates new x coordinate
(define (rotateX x y inAngle)
 (- (* x (cos inAngle)) (* y (sin inAngle))))

; calculates new y coordinate
(define (rotateY x y inAngle)
 (+ (* x (sin inAngle)) (* y (cos inAngle))))


; tree1 start coordinates
(define startX 250)
(define startY 0)

; number of iterations per tree
(define loopCount 25)

; used to decrease size of polygon
(define scaleAmount .85)

; draws tree
(define (makeTree depth rotateAmount currentScale currentXY startXY)
  (define newPoly (duplicatePolygon treePolygon))
   (when (> depth 0)
    (send newPoly scale currentScale currentScale)
    (send newPoly rotate rotateAmount)
    (send newPoly translate (car currentXY) (cdr currentXY))
    (drawToScreen newPoly)
    (set! polyCount (+ 1 polyCount))
    (define px (* (car startXY) currentScale))
    (define endX (rotateX px (cdr startXY) (- rotateAmount)))
    (define endY (rotateY px (cdr startXY) (- rotateAmount)))
    (define newX (+ endX (car currentXY)))
    (define newY (+ endY (cdr currentXY)))
    (cond 
    [(< depth 10) (set! currentScale (* (/ currentScale 5) (/ scaleAmount 5)))]
    [ else (set! currentScale (* currentScale scaleAmount))])
    (set! rotateAmount (- rotateAmount 0.0174533))
    (makeTree (- depth 1) rotateAmount currentScale (cons newX newY) startXY)
    (cond
    [ (= 0 (modulo depth 3)) (makeTree (- depth 1) (- rotateAmount) currentScale (cons newX newY) startXY)]
    [ (= 0(modulo depth 5)) (makeTree (- depth 1) (- 0.174533(- rotateAmount))  currentScale (cons newX newY) startXY)])))



; calculates zoom amount
(define zoomAmt .75)
(printf "zoom: ~v" zoomAmt)

; creates four tree objects
(define (makeForest startX startY)
  (makeTree loopCount -0.349066  1.0 (cons 0 0) (cons startX startY))
  (makeTree loopCount -0.331613 1.0 (cons -3 1142) (cons startX startY))
  (makeTree loopCount 3.40339  1.0 (cons 4100 0) (cons startX startY))
  (makeTree loopCount 3.31613 1.0 (cons 4100 1142) (cons startX startY)))


(define (makePictures numOfPics xMin xMax yMin yMax)
  (when (< numOfPics 11)
    (makeForest startX startY)
    (send myTarget save-file (string-append "pics\\myPic" (number->string numOfPics) ".png") 'png)
    (define newXMin(- (/ (+ xMin xMax) 2)(* zoomAmt (/ (- xMax xMin) 2))))
    (define newYMin(- (/ (+ yMin yMax) 2)(* zoomAmt (/ (- yMax yMin) 2))))
    (set! xScale (* xScale zoomAmt))
    (set! yScale (* yScale zoomAmt))
    (printf "scale: ~v~n" xScale)
    (set! xTrans (+(- xMax newXMin) xTrans))
    (set! yTrans (+ (- yMax newYMin) yTrans))
    (printf "xTrans: ~v~n" xTrans)
    (printf "yTrans: ~v~n" yTrans)
    (makePictures (+ numOfPics 1) newXMin (abs newXMin) newYMin (abs newYMin))))

(makeForest startX startY)
(trace makePictures)
(makePictures 1 -2048 2048 -1152 1152)
myTarget


; saves image to file
(send myTarget save-file "pics\\myPic.png" 'png)
