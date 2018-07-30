(in-package :mcclim-render-internals)

(defgeneric aa-render-draw-fn (image clip-region pixeled-design))
(defgeneric aa-render-draw-span-fn (image clip-region pixeled-design))
(defgeneric aa-render-xor-draw-fn (image clip-region pixeled-design))
(defgeneric aa-render-xor-draw-span-fn (image clip-region pixeled-design))
(defgeneric aa-render-alpha-draw-fn (image clip-region))
(defgeneric aa-render-alpha-draw-span-fn (image clip-region))

(defgeneric aa-cells-sweep/rectangle (image design state clip-region))
(defgeneric aa-cells-alpha-sweep/rectangle (image design state clip-region))

(defmethod aa-cells-sweep/rectangle ((image rgb-image-mixin) (ink pixeled-design) state clip-region)
  (let ((draw-function nil)
        (draw-span-function nil)
        (current-clip-region
         (if (rectanglep clip-region)
             nil
             clip-region)))
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	clip-region
      (setf draw-function
            (if (typep ink 'pixeled-flipping-design)
                (aa-render-xor-draw-fn image current-clip-region ink)
                (aa-render-draw-fn image current-clip-region ink)))
      (setf draw-span-function
            (if (typep ink 'pixeled-flipping-design)
                (aa-render-xor-draw-span-fn image current-clip-region ink)
                (aa-render-draw-span-fn image current-clip-region ink)))
      (%aa-cells-sweep/rectangle state
                                (floor min-x)
                                (floor min-y)
                                (ceiling max-x)
                                (ceiling max-y)
                                draw-function
                                draw-span-function))))

(defmethod aa-cells-alpha-sweep/rectangle ((image gray-image-mixin) ink state clip-region)
  (let ((draw-function nil)
        (draw-span-function nil)
        (current-clip-region
         (if (rectanglep clip-region)
             nil
             clip-region)))
    (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	clip-region
      (setf draw-function
            (aa-render-alpha-draw-fn image current-clip-region))
      (setf draw-span-function
            (aa-render-alpha-draw-span-fn image current-clip-region))
      (%aa-cells-sweep/rectangle state
                                (floor min-x)
                                (floor min-y)
                                (ceiling max-x)
                                (ceiling max-y)
                                draw-function
                                draw-span-function))))

(defun %aa-scanline-sweep (scanline function function-span &key start end)
  "Call FUNCTION for each pixel on the polygon covered by
SCANLINE. The pixels are scanned in increasing X. The sweep can
be limited to a range by START (included) or/and END (excluded)."
  (declare (optimize speed (debug 0) (safety 0) (space 2))
           (type (function (fixnum fixnum fixnum) *) function)
           (type (function (fixnum fixnum fixnum fixnum) *) function-span))
  (let ((x-min (aa::cell-x (car scanline)))
        (x-max (aa::cell-x (car scanline)))
        (cover 0)
        (y (aa::scanline-y scanline))
        (cells scanline)
        (last-x nil))
    (when start
      ;; skip initial cells that are before START
      (loop while (and cells (< (aa::cell-x (car cells)) start))
         do (incf cover (aa::cell-cover (car cells)))
         (setf last-x (aa::cell-x (car cells))
               cells (cdr cells))))
    (when cells
      (dolist (cell cells)
        (let ((x (aa::cell-x cell)))
          (when (and last-x (> x (1+ last-x)))
            (let ((alpha (aa::compute-alpha cover 0)))
              (unless (zerop alpha)
                (let ((start-x (if start (max start (1+ last-x)) (1+ last-x)))
                      (end-x (if end (min end x) x)))
                  (setf x-min (min x-min start-x))
                  (setf x-max (max x-max end-x))
                  (if function-span
                      (funcall function-span start-x end-x y alpha)
                      (loop for ix from start-x below end-x
                         do (funcall function ix y alpha)))))))
          (when (and end (>= x end))
            (return (values x-min x-max)))
          (incf cover (aa::cell-cover cell))
          (let ((alpha (aa::compute-alpha cover (aa::cell-area cell))))
            (unless (zerop alpha)
              (funcall function x y alpha)))
          (setf last-x x))))
    (values x-min x-max)))

(defun %aa-cells-sweep/rectangle (state x1 y1 x2 y2 function &optional function-span)
  "Call FUNCTION for each pixel on the polygon described by
previous call to LINE or LINE-F. The pixels are scanned in
increasing Y, then on increasing X. This is limited to the
rectangle region specified with (X1,Y1)-(X2,Y2) (where X2 must be
greater than X1 and Y2 must be greater than Y1, to describe a
non-empty region.)

For optimization purpose, the optional FUNCTION-SPAN, if
provided, is called for a full span of identical alpha pixel. If
not provided, a call is made to FUNCTION for each pixel in the
span."
  (let ((scanlines (aa::freeze-state state))
        (x-min x2)
        (x-max x1)
        (y-min y2)
        (y-max y1))
    (dolist (scanline scanlines)
      (setf y-min (min y-min (aa::scanline-y scanline)))
      (setf y-max (max y-max (aa::scanline-y scanline)))
      (when (<= y1 (aa::scanline-y scanline) (1- y2))
        (multiple-value-bind (xa xb)
            (%aa-scanline-sweep scanline function function-span :start x1 :end x2)
          (setf x-min (min x-min xa))
          (setf x-max (max x-max xb)))))
    (make-rectangle* x-min y-min (1+ x-max) (1+ y-max))))

