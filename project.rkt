#lang racket
(require gigls/unsafe)

; citations:
; https://docs.racket-lang.org/reference/Filesystem.html
; https://courses.cs.washington.edu/courses/cse413/12au/racket/side-effects.html
; https://docs.racket-lang.org/reference/bytestrings.html
; https://docs.racket-lang.org/reference/vectors.html

; set path to dir run from, any path works though
; swap out for 'exec-file for path of the executable
(define image-series 
  (lambda (n width height) 
    (let
      ([base-image (image-load (string-append (find-system-path 'origin-dir) 
					      (number->string (quotient n 100))
					      ".jpg"))])
    (random-seed (+ 
		   (irgb-red (image-get-pixel base-image 0 0))
		   (irgb-blue (image-get-pixel base-image 0 0))
		   (irgb-green (image-get-pixel base-image 0 0))
		   n))
    (triangleTesselationHueShift (modulo n 100) (base-image)))))

(define triangleTesselationHueShift
  (lambda (n inputImage)
    (letrec ([pointTrys 10]
	     [minDistance (sqr 3)]
	     [tesselatedImage (image-variant baseImage (lambda (aa) aa))] ; to avoid messing with the original
	     [finalImage (image-variant baseImage 
					(lambda (color) (hsv->irgb (hsv 
								     (+ (irgb->hue color) 
									(* (/ n 100) 360))
								     (irgb->saturation color)
								     (irgb->value color)))))]
	     [tesselatedReigon (let* ([sixthWidth (max 50 (/ (image-width inputImage) 6))]
				      [sixthHeight (max 50 (/ (image-height inputImage) 6))]
				      [x1 (random (- (image-width inputImage) sixthWidth))]
				      [y1 (random (- (image-height inputImage) sixthHeight))])
				 (cons (cons x1 y2) 
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
	     [tesselatedSize (cons (- (caar tesselatedReigon) (cdar tesselatedReigon))
				   (- (cdar tesselatedReigon) (cddr tesselatedReigon)))]

	     ;random points section
	     [numPoints (exact (sqrt (+ (car tesselatedSize) 
					(cdr tesselatedSize))))]
	     [randomPoints (make-vector numPoints 0)]
	     [setRandomPointsCheck (lambda (pos xx yy) 
				     (cond [(= pos 0) #t]
					   ;check distance 
					   [(> (+ (sqr (- (car (vector-ref randomPoints pos)) xx))
						  (sqr (- (cdr (vector-ref randomPoints pos)) yy)))
					       minDistance) #f]
					   [else (setRandomPointsCheck (- pos 1) xx yy)]))]
	     [setRandomPoint (lambda (pos try)
			       (let ([xx (random (car tesselatedSize))]
				     [yy (random (cdr tesselatedSize))])
				 (if (or (= try 0) (setRandomPointsCheck xx yy)) 
				   (cons xx yy)
				   (setRandomPoint pos (- try 1)))))]
	     [setRandomPoints (lambda (pos) 
				(cond [(< 0 pos) (vector-set! randomPoints pos (setRandomPoint (- pos 1)))
						 (setRandomPoints (- pos 1))]
				      [else 0]))]

	     ;dealing with connecting points
	     [connections null]
	     [twoDCrossProduct (lambda (pairA pairB) (- (* (car pairA) (cdr pairB)) (* (cdr pairA) (car pairB))))]
	     [checkSegmentsDontIntersect ;http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect returns #t if they don't intersect
	       (lambda (pp rr pointBA pointBB)
		 (let ([qq pointBA]
		       [ss (cons (- (cdr pointBB) (cdr pointBA)) (- (cdr pointBB) (cdr pointBA)))]
		       [rrCrossSs (twoDCrossProduct rr ss)])
		   (or (= 0 rrCrossSs) 
		       (let ([tt (/ (twoDCrossProduct (cons (- (car qq) (car pp)) (- (cdr qq) (cdr pp))) ss)
				    rrCrossSs)]) ; let statemets like this to avoid uneccisary calculation, as this gets called a lot
			 (or (< tt 0) (> 1 tt)))
		       (let ([uu (/ (twoDCrossProduct (cons (- (car pp) (car qq)) (- (cdr pp) (cdr qq))) rr)
				    (* -1 rrCrossSs))]) ; same as ss x rr
			 (or (< uu 0) (> uu 1))))))]
	     [checkSegmentCrosses (lambda (pp qq connectionsLeft) 
				    (cond [(null? connectionsLeft) #t]
					  [(not (checkSegmentsDontIntersect 
						  pp 
						  qq 
						  (vector-ref randomPoints (caar (connectionsLeft)))
						  (vector-ref randomPoints (cdar (connectionsLeft)))))
					   #f]
					  [else (checkSegmentCrosses pp qq (cdr connectionsLeft))]))]
	     [connectionsForPoint! (lambda (pointNum pos)
				    (cond [(= pos -1) 0]
					  [(not (checkSegmentCrosses 
						  (vector-ref randomPoints pointNum) 
						  (cons (- (cdr (vector-ref randomPoints pos))
							   (cdr (vector-ref randomPoints pointNum))) 
							(- (cdr (vector-ref randomPoints pos))
							   (cdr (vector-ref randomPoints pointNum))))
						  randomconnections))
					   (set! connections (cons (cons pointNum pos) connectons))
					   (connectionsForPoint! pointNum (- pos 1))]
					  [else (connectionsForPoint! pointNum (- pos 1))]))]
	     [findConnections! (lambda (pos) 
				 (if (= pos numPoints) 0 (connectionsForPoint! pos pos)))]

	     ;finding triangles
	     [connectionsMatrix (make-vector numPoints (make-vector numPoints #f))]
	     [initializeConnectionsMatrix! 
	       (lambda (lst) 
		 (cond [(null? lst) 0]
		       [else (vector-set! (vector-ref connectionsMatrix (caar (lst)) (cadr lst)) #t)
			     (vector-set! (vector-ref connectionsMatrix (cadr (lst)) (caar lst)) #t)
			     (initializeConnectionsMatrix! (car lst))]))]
	     [findTrianglesBottom (lambda (pos vec findVal) 
				    (cond [(= pos (vector-length vec)) null]
					  [(vector-ref vec pos) (cons pos 
								      (findTrianglesBottom 
									(+ pos 1) 
									vec 
									findVal))]
					  [else (findTrianglesBottom (+ pos 1) vec findVal)]))]
	     [findTrianglesMiddle (lambda (pos findVal)
				    (cond [(= pos (vector-length connectionsMatrix)) null]
					  [(vector-ref 
	     [findTrianglesTop (lambda (lst) (
	     )
      ;initialize vector
      (setRandomPoints numPoints)
      (findConnections! 0)
      (initializeConnectionsMatrix! connections)
    ))



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
