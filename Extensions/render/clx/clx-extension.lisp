(in-package :mcclim-render-internals)

;;; RGB-IMAGE support, from Closure

(defmethod medium-draw-image*
    ((medium sheet-with-medium-mixin) design x y)
  (medium-draw-image* (sheet-medium medium) design x y))

(defmethod medium-draw-image*
    ((medium clim-clx::clx-medium) (image image-mixin) x y)
  (let* ((da (clim-clx::sheet-xmirror (medium-sheet medium)))
	 (width (image-width image))
	 (height (image-height image))
         (region
          (region-intersection
           (climi::medium-device-region medium)
           (transform-region (sheet-device-transformation (medium-sheet medium))
                             (make-rectangle* x y (+ x width) (+ y height))))))
    (multiple-value-bind (x1 y1)
        (transform-position
         (sheet-device-transformation (medium-sheet medium))
         x y)
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
          region
        (let ((x0 (max 0 (round (+ 0 (- min-x x1)))))
              (y0 (max 0 (round (+ 0 (- min-y y1)))))
              (w (round (- max-x min-x)))
              (h (round (- max-y min-y))))
          (when (and (> w 0) (> h 0))
            (let ((pixmap (compute-rgb-image-pixmap da image x0 y0 w h)))
              (let ((gcontext (xlib:create-gcontext :drawable da)))
                (xlib:copy-area pixmap gcontext
                                0 0 w h
                                da
                                (round min-x) (round min-y))
                (xlib:free-pixmap pixmap)))))))))


(defmethod medium-draw-image*
    ((medium clim-clx::clx-medium) (image clx-rgb-image) x y)
  (let* ((da (clim-clx::sheet-xmirror (medium-sheet medium)))
	 (width (image-width image))
	 (height (image-height image))
         (region
          (region-intersection
           (climi::medium-device-region medium)
           (transform-region (sheet-device-transformation (medium-sheet medium))
                             (make-rectangle* x y (+ x width) (+ y height))))))
    (multiple-value-bind (x1 y1)
        (transform-position
         (sheet-device-transformation (medium-sheet medium))
         x y)
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
          region
        (let ((x0 (max 0 (round (+ 0 (- min-x x1)))))
              (y0 (max 0 (round (+ 0 (- min-y y1)))))
              (w (round (- max-x min-x)))
              (h (round (- max-y min-y))))
          (when (and (> w 0) (> h 0))
            (let ((pixmap (compute-rgb-image-pixmap2 da image x0 y0 w h width height)))
              (let ((gcontext (xlib:create-gcontext :drawable da)))
                (xlib:copy-area pixmap gcontext
                                0 0 w h
                                da
                                (round min-x) (round min-y))
                (xlib:free-pixmap pixmap)))))))))


(defun compute-rgb-image-pixmap (drawable image x0 y0 w h)
  (let* ((depth (xlib:drawable-depth drawable))
         (im (image-to-ximage-for-drawable drawable image x0 y0 w h)))
    (setf w (max w 1))
    (setf h (max h 1))
    (let* ((pixmap (xlib:create-pixmap :drawable drawable
				       :width w
				       :height h
				       :depth depth))
	   (gc (xlib:create-gcontext :drawable pixmap)))
      (unless (or (>= w 2048) (>= h 2048)) ;### CLX bug
	(xlib:put-image pixmap gc im
			:src-x 0 :src-y 0
			:x 0 :y 0
			:width w :height h))
      (xlib:free-gcontext gc)
      pixmap)))


(defun compute-rgb-image-pixmap2 (drawable image x0 y0 w h width height)
  (let* ((depth (xlib:drawable-depth drawable))
         (im (xlib:create-image :width  width
                                :height height
                                :depth  depth
				:bits-per-pixel 32
                                :data (image-pixels image)
                                :format :z-pixmap)))
    (setf w (max w 1))
    (setf h (max h 1))
    (let* ((pixmap (xlib:create-pixmap :drawable drawable
				       :width w
				       :height h
				       :depth depth))
	   (gc (xlib:create-gcontext :drawable pixmap)))
      (unless (or (>= w 2048) (>= h 2048)) ;### CLX bug
	(xlib:put-image pixmap gc im
			:src-x x0 :src-y y0
			:x 0 :y 0
			:width w :height h))
      (xlib:free-gcontext gc)
      pixmap)))


(defun image-to-ximage-for-drawable (drawable image x0 y0 w h)
  (image-to-ximage image
		   (xlib:drawable-depth drawable)
                   (xlib:window-colormap drawable)
                   x0 y0 w h))

(defun image-to-ximage (image depth colormap x0 y0 w h)
  (let* ((width (mcclim-render::image-width image))
         (height (mcclim-render::image-height image))
         (clx-image (make-instance 'clx-rgb-image :height h :width w :colormap colormap))
         (ximage (xlib:create-image :width  w
                                    :height h
                                    :depth  depth
				    :bits-per-pixel 32
                                    :data (image-pixels clx-image)
                                    :format :z-pixmap)))
    (copy-image image x0 y0 w h clx-image 0 0)
    #+nil (let ((oimage (mcclim-render:coerce-image clx-image :rgb :opticl)))
      (mcclim-render:write-image oimage "/tmp/a.jpg")) 
    ximage))
