(in-package :mcclim-render)

;;;
;;; 2D RGB
;;;
(deftype clim-rgb-image-data () '(simple-array (unsigned-byte 32) (* *)))

;;;
;;; Pixeled Design
;;;

(defmethod mcclim-raster::%make-pixeled-design ((ink mcclim-image::rgb-pattern))
  (let* ((img (slot-value ink 'mcclim-image::image)))
    (make-pixeled-image-design :image
                               (make-instance 'rgb-image
                                              :width (mcclim-image::image-width img)
                                              :height (mcclim-image::image-height img)
                                              :pixels (mcclim-image::image-data img)))))

;;;
;;; Operations
;;;

;;(make-copy-image rgb-image rgb-image)
;;(make-fill-image-without-stencil rgb-image)
;;(make-fill-image-with-stencil rgb-image 2d-stencil-image)

(defmethod coerce-image ((image basic-image) (image-class (eql 'mcclim-image::rgb-image)) &optional image-family)
  (if (typep image 'mcclim-image::rgb-image)
      image
      (let ((img (coerce-image image 'rgb-image)))
        (make-instance 'mcclim-image::rgb-image
                       :width (image-width img)
                       :height (image-height img)
                       :data (image-pixels img)))))

(defmethod coerce-image ((image mcclim-image::rgb-image) (image-class (eql 'rgb-image)) &optional image-family)
  (make-instance 'rgb-image
                 :width (mcclim-image::image-width image)
                 :height (mcclim-image::image-height image)
                 :pixels (mcclim-image::image-data image)))
