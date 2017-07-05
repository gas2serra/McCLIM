(in-package :mcclim-render)

(defclass stencil-image-mixin ()
  ())


(defgeneric make-get-rgba-octets-code (image-class pixels-var x-var y-var))
(defgeneric make-set-rgba-octets-code (image-class pixels-var x-var y-var red-var grren-var blue-var alpha-var))
(defgeneric make-get-alpha-octets-code (image-class pixels-var x-var y-var))
(defgeneric make-set-alpha-octets-code (image-class pixels-var x-var y-var red-var grren-var blue-var alpha-var))

;;;
;;; Image Pattern
;;;
(defclass image-pattern (climi::pattern image-design)
  ())

(defmethod pattern-width ((pattern image-pattern))
  (image-width (image pattern)))

(defmethod pattern-height ((pattern image-pattern))
  (image-height (image pattern)))

(defmethod climi::medium-draw-pattern* (medium (pattern image-pattern) x y)
  (medium-draw-image* medium (image pattern) x y))
