;PLWeek10
;Austin Riggs
;10/25/23

#lang racket

(require racket/draw)
(require colors)
(require racket/math)

; used by drawToScale for polygon scaling and tranformation
(define xScale .4)
(define yScale .4)
(define xTrans 256)
(define yTrans 144)


; height and width of background
(define imageWidth 512)
(define imageHeight 288)

; bitmap and draw context
(define myTarget (make-bitmap imageWidth imageHeight)) ; A bitmap
(define dc (new bitmap-dc% [bitmap myTarget])) ; a drawing context

;background square
(send dc set-pen "green" 2 'solid)
(send dc set-brush (make-color 15 23 77) 'solid) 
(send dc draw-rectangle
      0 0 
      512 288) 

;creation of polygon
(send dc set-pen "red" 1 'solid)
(send dc set-brush (make-color 250 250 10) 'solid)
(define myPolygon (new dc-path%)) 
(send myPolygon move-to -3 -3) 
(send myPolygon line-to  -5 5)
(send myPolygon line-to 5 5)
(send myPolygon line-to 3 -3)
(send myPolygon line-to -3 -3)
(send myPolygon close)

; drawToScreen function
(define (drawToScreen polygon)
  (send polygon scale xScale yScale)
  (send polygon translate xTrans yTrans)
  (send dc draw-path polygon)
  (send polygon translate (- xTrans) (- yTrans))
  (send polygon scale (/ 1 xScale) (/ 1 yScale)))

; first drawing of polygon
(drawToScreen myPolygon)

; second drawing of polygon
(send myPolygon translate 20 15)
(drawToScreen myPolygon)

;third drawing of polygon
(send myPolygon scale 3 2)
(drawToScreen myPolygon) 

; polygon translation
(send myPolygon translate 80 0) 

; loop for rotation and coloring of color wheel using polygon 
(define (colorWheel polygon)
(for ([i (in-range 90)])
    (define hue (/ i 90.0))
    (define myHsv (hsv hue 1.0 1.0))
    (send dc set-brush (hsv->color myHsv) 'solid)
    (send dc set-pen (hsv->color myHsv) 2 'solid)
    (send polygon rotate .1)
    (drawToScreen myPolygon)))



; call funciton
(colorWheel myPolygon)

; save image as png
(send myTarget save-file "myPic.png" 'png) 

; display image
myTarget

