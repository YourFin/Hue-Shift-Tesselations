#lang racket
(require gigls/unsafe)
(require racket/vector)

; citations:
; https://docs.racket-lang.org/reference/Filesystem.html
; https://courses.cs.washington.edu/courses/cse413/12au/racket/side-effects.html
; https://docs.racket-lang.org/reference/bytestrings.html
; https://docs.racket-lang.org/reference/vectors.html

; set path to dir run from, any path works though
; swap out for 'exec-file for path of the executable


;;; Procedure:
;;;   image-series
;;; Parameters:
;;;   n, an integer
;;;   width, an integer
;;;   height, an integer
;;; Purpose:
;;;   to produce a series of different, repeatable images for 1000 different values of n,
;;;   of width $width and height $height
;;;      each image will be a hueshifted version of a scenic landscape image, hueshifted
;;;      with a non-hueshifted area of random tesselated triangles the color of the 
;;;      non hue-shifted image in that area
;;; Produces:
;;;   output, an image
;;; Preconditions:
;;;   0 <= n < 1000
;;;   width > 300
;;;   height > 300
;;; Postconditions:
;;;   

(define image-series 
  (lambda (n width height) 
    (let
      ([base-image (image-load (string-append (path->string (find-system-path 'orig-dir))
					      (number->string (quotient n 100))
					      ".jpg"))])
    (random-seed (+ 
		   (irgb-red (image-get-pixel base-image 0 0))
		   (irgb-blue (image-get-pixel base-image 0 0))
		   (irgb-green (image-get-pixel base-image 0 0))
		   n))
    (gimp-image-scale base-image width height)
    (triangleTesselationHueShift (modulo n 100) base-image))))


;;;;;;; The following was written by Sam Rebelsky for this project, all credit to him for this:

;;; Procedure:
;;;   image-info
;;; Parameters:
;;;   image, an image id
;;; Purpose:
;;;   Compute some basic info on the image
;;; Produces:
;;;   info, a four element list containing
;;;     * count of non-white pixels
;;;     * total of red components of nwps
;;;     * total of green components of nwps
;;;     * total of blue components of nwps
(define image-info
  (lambda (image)
    (let ([count 0]
          [sum-red 0]
          [sum-green 0]
          [sum-blue 0]
          [white (irgb 255 255 255)])
      (image-transform! image (lambda (color) ; essentially image-compute
                                (when (not (eq? color white))
                                  (set! count (+ count 1))
                                  (set! sum-red (+ sum-red (irgb-red color)))
                                  (set! sum-green (+ sum-green (irgb-green color)))
                                  (set! sum-blue (+ sum-blue (irgb-blue color))))
                                color))
      (list count sum-red sum-green sum-blue))))

;;; Procedure:
;;;   selection->image
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Create a new image containing the selected part of image.
;;; Produces:
;;;   new-image, an image
(define selection->image
  (lambda (image)
    (let* ([bounds (gimp-selection-bounds image)]
           [x1 (cadr bounds)]
           [y1 (caddr bounds)]
           [x2 (list-ref bounds 3)]
           [y2 (list-ref bounds 4)]
           [width (- x2 x1)]
           [height (- y2 y1)]
           [result (image-new width height)])
      (gimp-edit-copy (image-get-layer image))
      (gimp-edit-paste (image-get-layer result) 1)
      (gimp-image-flatten result)
      result)))

;;; Procedure:
;;;   average-nwp
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Computes the average non-white pixel color in image
;;; Produces:
;;;   ave, a color
(define average-nwp
  (lambda (image)
    (let ([data (image-info image)])
      (irgb (/ (cadr data) (car data))
            (/ (caddr data) (car data))
            (/ (cadddr data) (car data))))))

;;;;;;;; end Code by Sam Rebelsky 

(define triangleTesselationHueShift
  (lambda (n inputImage)
    (letrec ([pointTrys 10]
	     [minDistance (sqr 3)]
	     [finalImage (image-variant inputImage 
					(lambda (color) (hsv->irgb (hsv 
								     (modulo (round (+ (irgb->hue color) 
										       (* (/ n 100) 360))) 360)
								     (irgb->saturation color)
								     (irgb->value color)))))]
	     [tesselatedReigon (let* ([sixthWidth (max 50 (floor (/ (image-width inputImage) 6)))]
				      [sixthHeight (max 50 (floor (/ (image-height inputImage) 6)))]
				      [x1 (random (- (image-width inputImage) sixthWidth))]
				      [y1 (random (- (image-height inputImage) sixthHeight))])
				 (cons (cons x1 y1) 
				       (cons (+ x1 
						sixthWidth 
						(random (- (image-width inputImage) 
							   x1 
							   sixthWidth)))
					     (+ y1 
						sixthHeight 
						(random (- (image-height inputImage) 
							   y1 
							   sixthHeight))))))]
	     [tesselatedSize (cons (- (cadr tesselatedReigon) (caar tesselatedReigon))
				   (- (cddr tesselatedReigon) (cdar tesselatedReigon)))]

	     ;random points section
	     [numPoints (inexact->exact (floor (sqrt (+ (car tesselatedSize) 
							(cdr tesselatedSize)))))]
	     [randomPoints (make-vector numPoints 0)]
	     [setRandomPointsCheck (lambda (pos xx yy) 
				     (cond [(< pos 0) #t]
					   ;check distance 
					   [(> (+ (sqr (- (car (vector-ref randomPoints pos)) xx))
						  (sqr (- (cdr (vector-ref randomPoints pos)) yy)))
					       minDistance) #f]
					   [else (setRandomPointsCheck (- pos 1) xx yy)]))]
	     [setRandomPoint (lambda (pos try)
			       (let ([xx (random (car tesselatedSize))]
				     [yy (random (cdr tesselatedSize))])
				 (if (or (= try 0) (setRandomPointsCheck (- pos 1) xx yy)) 
				   (cons xx yy)
				   (setRandomPoint pos (- try 1)))))]
	     [setRandomPoints (lambda (pos) 
				(cond [(< pos numPoints) (vector-set! randomPoints pos (setRandomPoint pos 10))
							 (setRandomPoints (+ pos 1))]
				      [else 0]))]

	     ;dealing with connecting points
	     [connections null]
	     [twoDCrossProduct (lambda (pairA pairB) (- (* (car pairA) (cdr pairB)) (* (cdr pairA) (car pairB))))]

	     ;credit to Titus Klingte for the following: 
	     [cross-product
	       (lambda (point1 point2)
		 (let ([ax (car point1)]
		       [ay (cdr point1)]
		       [bx (car point2)]
		       [by [cdr point2]])
		   (- (* ax by) (* bx ay))))]

	     ;https://martin-thoma.com/how-to-check-if-two-line-segments-intersect/
	     [boundingRectanglesIntersect?
	       (lambda (pointAA pointAB pointBA pointBB)
		 (let ([xA1 (min (car pointAA) (car pointAB))]
		       [yA1 (min (cdr pointAA) (cdr pointAB))]
		       [xA2 (max (car pointAA) (car pointAB))]
		       [yA2 (max (cdr pointAA) (cdr pointAB))]
		       [xB1 (min (car pointBA) (car pointBB))]
		       [yB1 (min (cdr pointBA) (cdr pointBB))]
		       [xB2 (max (car pointBA) (car pointBB))]
		       [yB2 (max (cdr pointBA) (cdr pointBB))])
		   (and (<= xA1 xB2)
			(>= xA2 xB1)
			(<= yA1 yB2)
			(>= yA2 yB1))))]
	     ;we still want to draw it if it's on the line
	     ;	     [pointOnLine? 
	     ;	       (lambda (segment point)
	     ;		 (let* ([EPSILON 1]
	     ;			[aTmp (cons (cons 0 0) 
	     ;				    (cons (- (cadr segment) (caar segment)) (- (cddr segment) (cdar segment))))]
	     ;			[bTmp (cons 
	     ;				(- (car point) (caar segment))
	     ;				(- (cdr point) (cdar segment)))])
	     ;		   (< 
	     [isPointRightOfLine 
	       (lambda (segment point)
		 (let* ([aTmp (cons (cons 0 0) 
				    (cons (- (cadr segment) (caar segment)) (- (cddr segment) (cdar segment))))]
			[bTmp (cons 
				(- (car point) (caar segment))
				(- (cdr point) (cdar segment)))])
		   (< (cross-product (cdr aTmp) bTmp) 0)))]
	     [lineSegmentCrossesLine? 
	       (lambda (segmentA segmentB)
		 (xor (isPointRightOfLine segmentA (car segmentB))
		      (isPointRightOfLine segmentA (cdr segmentB))))]
	     [segmentsIntersect? 
	       (lambda (pointAA pointAB pointBA pointBB)
		 (and (boundingRectanglesIntersect? pointAA pointAB pointBA pointBB)
		      (lineSegmentCrossesLine? (cons pointAA pointAB) (cons pointBA pointBB))
		      (lineSegmentCrossesLine? (cons pointBA pointBB) (cons pointAA pointAB))))]

	     [checkSegmentCrosses (lambda (point1 point2 connectionsLeft) 
				    (cond [(null? connectionsLeft) #t]
					  [(segmentsIntersect? point1 
							       point2 
							       (vector-ref randomPoints (caar connectionsLeft)) 
							       (vector-ref randomPoints (cdar connectionsLeft)))
					   #f]
					  [else 
					    (checkSegmentCrosses point1 point2 (cdr connectionsLeft))]))]
	     [connectionsForPoint 
	       (lambda (pointNum pos lstSoFar formerList)
		 (cond [(= pos -1) lstSoFar]
		       [(checkSegmentCrosses 
			  (vector-ref randomPoints pos) 
			  (vector-ref randomPoints pointNum)
			  (filter (lambda (point) ; dump segments that share an endpoint
				    (let ([check (lambda (num) 
						   (not (or (= num pos) (= num pointNum))))])
				      (and (check (car point)) (check (cdr point)))))
				  formerList))
			(connectionsForPoint 
			  pointNum 
			  (- pos 1) 
			  (cons (cons pointNum pos) lstSoFar)
			  formerList)]
		       [else (connectionsForPoint
			       pointNum 
			       (- pos 1) 
			       lstSoFar
			       formerList)]))]
	     [findConnections (lambda (pos lstSoFar) 
				(if (< pos numPoints) 
				  (findConnections 
				    (+ pos 1) 
				    (append (connectionsForPoint 
					      pos 
					      (- pos 1) 
					      null 
					      lstSoFar) 
					    lstSoFar))
				  lstSoFar))]

	     ;finding triangles
	     [connectionsMatrix (make-vector (* numPoints numPoints) #f)]
	     [makePointTrueConnectionsMatrix!
	       (lambda (col row)
		 (vector-set! connectionsMatrix (+ (* row numPoints) col) #t))]
	     [connectionsMatrixPoint
	       (lambda (col row)
		 (vector-ref connectionsMatrix (+ (* row numPoints) col)))]
	     [subConnectionsMatrix
	       (lambda (row)
		 (vector-copy connectionsMatrix (* row numPoints) (* (+ row 1) numPoints)))]
	     [initializeConnectionsMatrix! 
	       (lambda (lst) 
		 (cond [(null? lst) 0]
		       [else 
			 (makePointTrueConnectionsMatrix! (caar lst) (cdar lst))
			 (makePointTrueConnectionsMatrix! (cdar lst) (caar lst))
			 (initializeConnectionsMatrix! (cdr lst))]))]
	     [vector->indexList 
	       (lambda (vec pos lstSoFar)
		 (cond [(= (vector-length vec) pos) lstSoFar]
		       [(vector-ref vec pos) 
			(vector->indexList vec (+ pos 1) (cons pos lstSoFar))]
		       [else (vector->indexList vec (+ pos 1) lstSoFar)]))]
	     [iotaCMleft
	       (lambda (pos)
		 (map (l-s + pos) (iota (- (vector-length connectionsMatrix) pos))))]
	     [trianglesBottom
	       (lambda (row findVal) 
		 (map (section list findVal row <>) 
		      (filter (lambda (index) 
				(and (connectionsMatrixPoint index findVal)
				     (< row index)))
			      (vector->indexList (subConnectionsMatrix row) 0 null))))]
	     [trianglesMiddle 
	       (lambda (row)
		 (apply append (map (compose (section trianglesBottom <> row) (section + row 1 <>)) (iota (- numPoints row 1)))))]

	     ;drawing
	     [drawTriangle!
	       (lambda (triangle)
		 (let ([points (map
				 (lambda (index)
				   (cons
				     (+ (car (vector-ref randomPoints index)) (caar tesselatedReigon))
				     (+ (cdr (vector-ref randomPoints index)) (cdar tesselatedReigon)))) 
				 triangle)])
		 (context-set-bgcolor! (irgb 255 255 255))
		 (context-set-fgcolor! (irgb 255 255 255))
		 (image-select-polygon! inputImage REPLACE points)
		 (context-set-fgcolor! (average-nwp (selection->image inputImage)))
		 (image-select-polygon! finalImage REPLACE points)
		 (image-fill-selection! finalImage)))]
	     )

	     ;initialize vector
	     (setRandomPoints 0)

	     ;initalize array for triangle finding
	     (initializeConnectionsMatrix! (findConnections 0 null))

	     ;draw triangles
	     (map drawTriangle! (apply append (map trianglesMiddle (iota numPoints))))

	     finalImage
	     )))

