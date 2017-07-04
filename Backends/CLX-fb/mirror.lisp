(in-package :clim-clx-fb)

(defclass clx-fb-mirror (fb-mirror)
  ())

(defmethod clim-clx::port-set-mirror-region ((port clx-fb-port) (mirror clx-fb-mirror) mirror-region)
  (clim-clx::port-set-mirror-region port (fb-mirror-real-mirror mirror) mirror-region))

(defmethod clim-clx::port-set-mirror-transformation
    ((port clx-fb-port) (mirror clx-fb-mirror) mirror-transformation)
  (clim-clx::port-set-mirror-transformation port (fb-mirror-real-mirror mirror)
                                            mirror-transformation))

;;;
;;;
;;;

(declaim (inline xlib-image-data-set-pixel))
(defun xlib-image-data-set-pixel (data x y red green blue alpha)
  (setf (aref data y x)
       (dpb blue (byte 8 0)
            (dpb green (byte 8 8)
                 (dpb red (byte 8 16)
                      (dpb alpha (byte 8 24) 0))))))


;;;
;;;
;;;

(defmethod fb-mirror-create-buffer ((mirror clx-fb-mirror) width height)
  (let ((data (make-array (list height width)
                          :element-type '(unsigned-byte 32)
                          :initial-element #x00FFFFFF)))
    (setf (fb-mirror-buffer mirror)
          (list
           data
           (xlib:create-gcontext :drawable (fb-mirror-real-mirror mirror)
                                 :background (values 0 0 0)
                                 :foreground (values 255 255 255))
           (xlib:create-image :bits-per-pixel 32
                              :data data
                              :depth 24
                              :width width
                              :height height
                              :format :z-pixmap)))))

(defmethod fb-mirror-destroy-buffer ((mirror clx-fb-mirror))
  ;;(xlib:destroy-image (third (fb-mirror-buffer mirror)))
  (xlib:free-gcontext (second (fb-mirror-buffer mirror)))
  (setf (fb-mirror-buffer mirror) nil))

(defmethod fb-mirror-copy-to-buffer ((mirror clx-fb-mirror) image region-set)
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
                   (xlib-image-data-set-pixel buffer x y red green blue alpha))))))
       region-set))))

(defmethod fb-mirror-flush-buffer ((mirror clx-fb-mirror) region-set)
  (with-slots (mcclim-render::image-lock gcontext)
      mirror
    (let ((clx-image (third (fb-mirror-buffer mirror)))
          (xmirror (fb-mirror-real-mirror mirror))
          (gcontext (second (fb-mirror-buffer mirror))))
      (when (and clx-image gcontext)
        (map-over-region-set-regions #'(lambda (region)
                                         (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
                                           region
                                           (let ((width (round (- max-x min-x)))
                                                 (height (round (- max-y min-y))))
                                             (when (and xmirror clx-image)
                                               (xlib::put-image xmirror
                                                                gcontext
                                                                clx-image
                                                                :src-x (round (max min-x 0)) :src-y (round (max min-y 0))
                                                                :x (round (max min-x 0)) :y (round (max min-y 0))
                                                                :width  (max 0 (- width (min 0 (- min-x))))
                                                                :height (max 0 (- height (min 0 (- min-y)))))))))
                                     region-set)))))

(defmethod fb-mirror-set-wm-size-hints ((mirror fb-mirror) width height space-requirement)
  (setf (xlib:wm-normal-hints (sheet-direct-xmirror mirror))
        (xlib:make-wm-size-hints 
         :width (round width)
         :height (round height)
         :max-width (min 65535 (round (space-requirement-max-width space-requirement)))
         :max-height (min 65535 (round (space-requirement-max-height space-requirement)))
         :min-width (round (space-requirement-min-width space-requirement))
         :min-height (round (space-requirement-min-height space-requirement)))))
