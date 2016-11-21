#lang racket
(require gigls/unsafe)

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

