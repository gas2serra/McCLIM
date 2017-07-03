(in-package :clim-sdl2)

(defclass sdl2-fb-mirror (fb-mirror)
  ((port :initarg :port)
   (render :initarg :render)))

;;;
;;;
;;;

(declaim (inline xlib-image-data-set-pixel))
(defun xlib-image-data-set-pixel (data width height x y red green blue alpha)
 (cffi-sys:%mem-set 
	(dpb blue (byte 8 0)
	     (dpb green (byte 8 8)
		  (dpb red (byte 8 16)
		       (dpb alpha (byte 8 24) 0))))
        data :UNSIGNED-INT (* 4 (+ (* y width) x))))

;;;
;;;
;;;

(defmethod fb-mirror-create-buffer ((mirror sdl2-fb-mirror) width height)
  (with-slots (port render)
      mirror
    (with-slots (mirror->window)
        port
      (let* ((win (gethash (fb-mirror-real-mirror mirror) mirror->window))
             (surface (<+ `(sdl2:create-rgb-surface ,width ,height 32
                                                    :r-mask #x000000ff
                                                    :g-mask #x0000ff00
                                                    :b-mask #x00ff0000
                                                    :a-mask #xff000000))))
        (setf (fb-mirror-buffer mirror)
              (list
               (<+ `(sdl2:surface-pixels ,surface))
               surface))))))

(defmethod fb-mirror-destroy-buffer ((mirror sdl2-fb-mirror))
  )

(defmethod fb-mirror-copy-to-buffer ((mirror sdl2-fb-mirror) image region-set)
  (format *debug-io* ">> COPY ~A~%" region-set)
   (let ((pixels (mcclim-render::image-pixels image))
        (buffer (car (fb-mirror-buffer mirror)))
        (width (image-width image))
        (height (image-height image)))
    (declare (type mcclim-render::opticl-rgb-image-data pixels))
    (when buffer
      (map-over-region-set-regions
       #'(lambda (region)
           (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
             region
             (let ((x1 (max 0 min-x))
                   (y1 (max 0 min-y))
                   (x2 (min max-x width))
                   (y2 (min max-y height)))
               (opticl:do-region-pixels (y x y1 x1 y2 x2)
                 pixels
                 (multiple-value-bind (red green blue alpha)
                     (opticl:pixel pixels y x)
                   (xlib-image-data-set-pixel buffer width height x y red green blue alpha))))))
       region-set))))

(defmethod fb-mirror-flush-buffer ((mirror sdl2-fb-mirror) region-set)
  (format *debug-io* ">> FLUSH ~A~%" region-set)
   (with-slots (port mcclim-render::image-lock render)
       mirror
     (with-slots (mirror->window)
         port
       (let ((surface (second (fb-mirror-buffer mirror)))
             (win (gethash (fb-mirror-real-mirror mirror) mirror->window)))
         (when (and surface)
           (let ((texture (sdl2:create-texture-from-surface render surface)))
             (map-over-region-set-regions #'(lambda (region)
                                              (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
                                                  region
                                                (let ((width (round (- max-x min-x)))
                                                      (height (round (- max-y min-y))))
                                                  (when (and mirror)
                                                    (sdl2:with-rects
                                                        ((src (round (max min-x 0))
                                                              (round (max min-y 0))
                                                              (max 0 (- width (max 0 (- min-x))))
                                                              (max 0 (- height (max 0 (- min-y))))))
                                                      (sdl2:render-copy render
                                                                        texture :source-rect src
                                                                        :dest-rect src))))))
                                          region-set)
             (sdl2::sdl-destroy-texture texture)
             (sdl2:render-present render)))))))

(defmethod fb-mirror-set-wm-size-hints ((mirror sdl2-fb-mirror) width height space-requirement)
  nil)
