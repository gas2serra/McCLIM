(in-package :mcclim-render-internals)

#+nil (declaim (optimize speed))

;;;
;;; Image
;;;
(defclass image ()
  ())

(defgeneric image-with (image))
(defgeneric image-height (image))

;;;
;;; Image mixins
;;;
(defclass image-mixin ()
  ())

(defclass image-adapter-mixin (image-mixin)
  ((img :initarg :image)))

(defclass rgba-image-mixin (image-mixin)
  ())

(defclass rgb-image-mixin (image-mixin)
  ())

(defclass gray-image-mixin (image-mixin)
  ())

(deftype image-rgba-get-fn () '(function (fixnum fixnum) (values octet octet octet octet)))
(defgeneric image-rgba-get-fn (image &key dx dy region))
(deftype image-rgba-set-fn () '(function (fixnum fixnum octet octet octet octet)))
(defgeneric image-rgba-set-fn (image &key dx dy))
(deftype image-rgb-get-fn () '(function (fixnum fixnum) (values octet octet octet)))
(defgeneric image-rgb-get-fn (image &key dx dy region))
(deftype image-rgb-set-fn () '(function (fixnum fixnum octet octet octet)))
(defgeneric image-rgb-set-fn (image &key dx dy))
(deftype image-gray-get-fn () '(function (fixnum fixnum) octet))
(defgeneric image-gray-get-fn (image &key dx dy region))
(deftype image-gray-set-fn () '(function (fixnum fixnum octet)))
(defgeneric image-gray-set-fn (image &key dx dy))

;; defaults
(defmethod image-rgba-get-fn ((image rgb-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-rgb-get-fn image :dx dx :dy dy :region region)))
    (declare (type image-rgb-get-fn fn))
    (lambda (x y)
      (declare (type fixnum x y))
      (multiple-value-bind (r g b)
          (funcall fn x y)
        (rgb->rgba r g b)))))
(defmethod image-rgba-get-fn ((image gray-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-gray-get-fn image :dx dx :dy dy :region region)))
    (declare (type image-gray-get-fn fn))
    (lambda (x y)
      (declare (type fixnum x y))
      (gray->rgba (funcall fn x y)))))
(defmethod image-rgba-set-fn ((image rgb-image-mixin) &key (dx 0) (dy 0))
  (let ((fn (image-rgb-set-fn image :dx dx :dy dy)))
    (declare (type image-rgb-set-fn fn))
    (lambda (x y red green blue alpha)
      (declare (type fixnum x y)
               (type octet red green blue alpha))
      (multiple-value-bind (r g b)
          (rgba->rgb red green blue alpha)
        (funcall fn x y r g b)))))
(defmethod image-rgba-set-fn ((image gray-image-mixin) &key (dx 0) (dy 0))
  (let ((fn (image-gray-set-fn image :dx dx :dy dy)))
    (declare (type image-gray-set-fn fn))
    (lambda (x y red green blue alpha)
      (declare (type fixnum x y)
               (type octet red green blue alpha))
      (multiple-value-bind (g)
          (rgba->gray red green blue alpha)
        (funcall fn x y g)))))
(defmethod image-rgb-get-fn ((image rgba-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-rgba-get-fn image :dx dx :dy dy :region region)))
    (declare (type image-rgba-get-fn fn))
    (lambda (x y)
      (declare (type fixnum x y))
      (multiple-value-bind (r g b a)
          (funcall fn x y)
        (rgba->rgb r g b a)))))
(defmethod image-rgb-get-fn ((image gray-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-gray-get-fn image :dx dx :dy dy :region region)))
    (declare (type image-gray-get-fn fn))
    (lambda (x y)
      (declare (type fixnum x y))
      (gray->rgb (funcall fn x y)))))
(defmethod image-rgb-set-fn ((image rgba-image-mixin) &key (dx 0) (dy 0))
  (let ((fn (image-rgba-set-fn image :dx dx :dy dy)))
    (declare (type image-rgba-set-fn fn))
    (lambda (x y red green blue)
      (declare (type fixnum x y)
               (type octet red green blue))
      (multiple-value-bind (r g b a)
          (rgb->rgba red green blue)
        (funcall fn x y r g b a)))))
(defmethod image-rgb-set-fn ((image gray-image-mixin) &key (dx 0) (dy 0))
  (let ((fn (image-gray-set-fn image :dx dx :dy dy)))
    (declare (type image-gray-set-fn fn))
    (lambda (x y red green blue)
      (declare (type fixnum x y)
               (type octet red green blue))
      (multiple-value-bind (g)
          (rgb->gray red green blue)
        (funcall fn x y g)))))
(defmethod image-gray-get-fn ((image rgba-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-rgba-get-fn image :dx dx :dy dy :region region)))
    (declare (type image-rgba-get-fn fn))
    (lambda (x y)
      (declare (type fixnum x y))
      (multiple-value-bind (r g b a)
          (funcall fn x y)
        (rgba->gray r g b a)))))
(defmethod image-gray-get-fn ((image  rgb-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-rgb-get-fn image :dx dx :dy dy :region region)))
    (declare (type image-rgb-get-fn fn))
    (lambda (x y)
      (declare (type fixnum x y))
      (multiple-value-bind (r g b)
          (funcall fn x y)
        (rgb->gray r g b)))))
(defmethod image-gray-set-fn ((image rgba-image-mixin) &key (dx 0) (dy 0))
  (let ((fn (image-rgba-set-fn image :dx dx :dy dy)))
    (declare (type image-rgba-set-fn fn))
    (lambda (x y gray)
      (declare (type fixnum x y)
               (type octet gray))
      (multiple-value-bind (r g b a)
          (gray->rgba gray)
        (funcall fn x y r g b a)))))
(defmethod image-gray-set-fn ((image rgb-image-mixin) &key (dx 0) (dy 0))
  (let ((fn (image-rgb-set-fn image :dx dx :dy dy)))
    (declare (type image-rgb-set-fn fn))
    (lambda (x y gray)
      (declare (type fixnum x y)
               (type octet gray))
      (multiple-value-bind (r g b)
          (gray->rgb gray)
        (funcall fn x y r g b)))))

(defgeneric image-type (image)
  (:method ((image rgba-image-mixin))
    :rgba)
  (:method ((image rgb-image-mixin))
    :rgb)
  (:method ((image gray-image-mixin))
    :gray))

(defvar *default-image-medium* :two-dim-array)
(defgeneric image-medium (image))

;;;
;;; Drawable Image
;;;

(defun draw-image* (medium image x y
                    &rest args
                    &key clipping-region transformation)
  (declare (ignorable clipping-region transformation args))
  (climi::with-medium-options (medium args)
    (medium-draw-image* medium image x y)))

(clim-internals::def-graphic-op draw-image* (image x y))

;;;
;;; Image Design
;;;
(defclass image-design (design)
  ((image :reader image
          :initarg :image)))

(defun make-image-design (image)
  (make-instance 'image-design :image image))

(defmethod clim:draw-design
    (medium (design image-design) &rest options
     &key (x 0) (y 0) &allow-other-keys)
  (climi::with-medium-options (medium options)
    (medium-draw-image* medium (slot-value design 'image) x y)))

;;;
;;; Image Pattern
;;;
(defclass image-pattern (pattern image-design)
  ())

(defmethod pattern-width ((pattern image-pattern))
  (image-width (image pattern)))

(defmethod pattern-height ((pattern image-pattern))
  (image-height (image pattern)))

(defmethod climi::medium-draw-pattern* (medium (pattern image-pattern) x y)
  (medium-draw-image* medium (image pattern) x y))

;;;
;;; Basic Image
;;;
(defclass basic-image (image)
  ((width :initform 0 :initarg :width :reader image-width :type fixnum)
   (height :initform 0 :initarg :height :reader image-height :type fixnum)
   (pixels :initarg :pixels
           :accessor image-pixels)))

;;;
;;; image I/O
;;;
(defgeneric read-image (source &key format type medium))
(defgeneric write-image (image destination &key format quality))

(defvar *image-file-readers* (make-hash-table :test 'equalp)
  "A hash table mapping keyword symbols naming image
formats to a function that can read an image of that format. The
functions will be called with one argument, the pathname of the
file to be read.")

(defvar *image-file-writer* (make-hash-table :test 'equalp)
  "A hash table mapping keyword symbols naming image
formats to a function that can write an image of that format. The
functions will be called with two arguments, the image and the pathname of the
file to be read.")

(defmacro define-image-file-reader (format (&rest args) &body body)
  `(setf (gethash ,format *image-file-readers*)
         #'(lambda (,@args)
             ,@body)))

(defun image-format-read-supported-p (format)
  "Return true if FORMAT is supported by `read-image'."
  (not (null (gethash format *image-file-readers*))))

(defmacro define-image-file-writer (format (&rest args) &body body)
  `(setf (gethash ,format *image-file-writer*)
         #'(lambda (,@args)
             ,@body)))

(defun image-format-write-supported-p (format)
  "Return true if FORMAT is supported by `write-image'."
  (not (null (gethash format *image-file-writer*))))

;;;
;;; Image operations
;;;
(defgeneric make-image (medium type width height))
(defgeneric coerce-image (image type &optional medium))
(defgeneric clone-image (image type &optional medium))
(defgeneric copy-image (src-image sx sy width height dst-image x y))
(defgeneric blend-image (src-image sx sy width height dst-image x y &key alpha))
(defgeneric crop-image (image sx sy width height &optional type medium))
(defgeneric fill-image (image design stencil &key x y width height stencil-dx stencil-dy))
(defgeneric coerce-alpha-channel (image &optional type medium))
(defgeneric clone-alpha-channel (image &optional type medium))
(defgeneric copy-alpha-channel (src-image sx sy width height dst-image x y))
