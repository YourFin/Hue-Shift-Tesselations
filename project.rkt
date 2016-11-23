#lang racket
(require gigls/unsafe)

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
      (image-transform! image (lambda (color)
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
	     [tesselatedImage (image-variant inputImage (lambda (aa) aa))] ; to avoid messing with the original
;	     [finalImage (image-variant inputImage 
;					(lambda (color) (hsv->irgb (hsv 
;								     (+ (irgb->hue color) 
;									(* (/ n 100) 360))
;								     (irgb->saturation color)
;								     (irgb->value color)))))]
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
				     (display "here")
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
							 (display tesselatedSize)
							 (setRandomPoints (+ pos 1))]
				      [else 0]))]

	     ;dealing with connecting points
	     [connections null]
	     [twoDCrossProduct (lambda (pairA pairB) (- (* (car pairA) (cdr pairB)) (* (cdr pairA) (car pairB))))]
	     [checkSegmentsDontIntersect ;http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect returns #t if they don't intersect
	       (lambda (pp rr pointBA pointBB)
		 (let* ([qq pointBA]
			[ss (cons (- (car pointBA) (car pointBB)) (- (cdr pointBA) (cdr pointBB)))]
			[rrCrossSs (twoDCrossProduct rr ss)])
		   (or (= 0 rrCrossSs) 
		       (let ([tt (/ (twoDCrossProduct (cons (- (car qq) (car pp)) (- (cdr qq) (cdr pp))) ss)
				    rrCrossSs)]) ; let statemets like this to avoid uneccisary calculation, as this gets called a lot
			 (display tt)
			 (or (< tt 0) (> tt 1)))
		       (let ([uu (/ (twoDCrossProduct (cons (- (car pp) (car qq)) (- (cdr pp) (cdr qq))) rr)
				    (* -1 rrCrossSs))]) ; same as ss x rr
			 (or (< uu 0) (> uu 1))))))]

	     [checkSegmentCrosses (lambda (pp rr connectionsLeft) 
				    (cond [(null? connectionsLeft) #t]
					  [(not (checkSegmentsDontIntersect 
						  pp 
						  rr 
						  (vector-ref randomPoints (caar (connectionsLeft)))
						  (vector-ref randomPoints (cdar (connectionsLeft)))))
					   #f]
					  [else (checkSegmentCrosses pp rr (cdr connectionsLeft))]))]
	     [connectionsForPoint! (lambda (pointNum pos)
				     (cond [(= pos -1) 0]
					   [(not (checkSegmentCrosses 
						   (vector-ref randomPoints pos) 
						   (cons (- (car (vector-ref randomPoints pos))
							    (car (vector-ref randomPoints pointNum))) 
							 (- (cdr (vector-ref randomPoints pos))
							    (cdr (vector-ref randomPoints pointNum))))
						   connections))
					    (set! connections (cons (cons pointNum pos) connections))
					    (connectionsForPoint! pointNum (- pos 1))]
					   [else (connectionsForPoint! pointNum (- pos 1))]))]
	     [findConnections! (lambda (pos) 
				 (when (> pos numPoints) 
				   (connectionsForPoint! pos pos) 
				   (findConnections! (+ pos 1))))]

	     ;finding triangles
	     [connectionsMatrix (make-vector numPoints (make-vector numPoints #f))]
	     [initializeConnectionsMatrix! 
	       (lambda (lst) 
		 (cond [(null? lst) 0]
		       [else (vector-set! (vector-ref connectionsMatrix (caar (lst)) (cadr lst)) #t)
			     (vector-set! (vector-ref connectionsMatrix (cadr (lst)) (caar lst)) #t)
			     (initializeConnectionsMatrix! (car lst))]))]
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
		 (filter (and (lambda (num) (vector-ref (vector-ref connectionsMatrix num) findVal))
			      (l-s < row))
			 (vector->indexList (vector-ref connectionsMatrix row) 0 null)))]
	     [trianglesMiddle 
	       (lambda (row findVal)
		 (apply append 
			(map (compose (l-s cons row) (section trianglesBottom <> findVal)) 
			     (filter (l-s < findVal) (vector->indexList (vector-ref connectionsMatrix row) 0 null)))))]
	     [trianglesTop 
	       (lambda (row)
		 (apply append ; remove a layer of list
			(map (compose (l-s cons row) (section trianglesMiddle <> row))
			     (filter (l-s < row) (vector->indexList (vector-ref connectionsMatrix row) 0 null)))))]
	     )

	     ;initialize vector
	     (display (checkSegmentsDontIntersect (cons 0 0) (cons 1 1) (cons 0 1) (cons 1 0)))
	     (display (twoDCrossProduct (cons 0 -1) (cons 1 1)))
;	     (display tesselatedSize)
;	     (newline)
;	     (setRandomPoints 0)
;	     (display randomPoints)
;	     (newline)
;	     (findConnections! 0)
;	     (display connections)
;	     (initializeConnectionsMatrix! connections)
;	     (newline)
;	     (display (apply append (map trianglesTop (iota numPoints))))
	     )))



;;;;   
;;;;   (define imageVector (make-vector (* (image-height image) (image-width image)) 0))
;;;;   ; initialize 2d vector
;;;;   ; ((lambda (pos) (if (= pos (image-height image)) 0 (vector-set! imageVector pos (make-vector (image-width image) 0)))) 0)
;;;;   
;;;;   ; fill with color values
;;;;   ; (let ([pos 0])
;;;;   ;   (image-variant 
;;;;   ;     image 
;;;;   ;     (lambda (color) (vector-set! pos color) (set! pos (+ pos 1) 0))))
;;;;   
;;;;   (define psudoRandomBytes "aa")
;;;;   (define psudoRandomFactory (thunk 
;;;;   			     (set! psudoRandomBytes (subbytes (sha256 psudoRandomBytes) 0 2)) 
;;;;   			     (bytes-ref psudoRandomBytes 0)))
