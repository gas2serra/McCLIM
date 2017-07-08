(in-package :mcclim-raster-internals)

;;;
;;; Image
;;;
(defclass image ()
  ((width :initform 0 :initarg :width :accessor image-width :type fixnum)
   (height :initform 0 :initarg :height :accessor image-height :type fixnum)))

;;;
;;; Image mixins
;;;
(defclass image-mixin ()
  ())

(defclass rgba-image-mixin (image-mixin)
  ())

(defclass rgb-image-mixin (image-mixin)
  ())

(defclass gray-image-mixin (image-mixin)
  ())

(defgeneric find-image-class (image type))

;;;
;;; Drawable Image
;;;
(defclass drawable-image (image)
  ())

(defgeneric map-rgb-color (drawable-image fn &key x y width height))

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
;;; Basic Image
;;;
(defclass basic-image (image)
  ((pixels :initarg :pixels
           :accessor image-pixels)))

(defgeneric image-pixels-type (image-class))

;;;
;;; image manipulation functions
;;;

(defgeneric image-pixels-type (image-class))

;;rgba
(defgeneric image-rgba-get-code (image-class pixels-var x-var y-var))
(defgeneric image-rgba-set-code (image-class pixels-var x-var y-var
                                 red-var green-var blue-var alpha-var))
(defgeneric image-rgba-blend-code (image-class pixels-var x-var y-var
                                   red-var green-var blue-var alpha-var))
(defgeneric image-gray-alpha-get-code (image-class pixels-var x-var y-var))
;; rgb
(defgeneric image-rgb-get-code (image-class pixels-var x-var y-var))
(defgeneric image-rgb-set-code (image-class pixels-var x-var y-var
                                red-var green-var blue-var))
(defgeneric image-rgb-blend-code (image-class pixels-var x-var y-var
                                  red-var green-var blue-var alpha-var))
;; gray
(defgeneric image-gray-get-code (image-class pixels-var x-var y-var))

(defgeneric image-gray-set-code (image-class pixels-var x-var y-var
                                 gray-var))
(defgeneric image-gray-blend-code (image-class pixels-var x-var y-var
                                   gray-var alpha-var))
;; alpha
(defgeneric image-alpha-get-code (image-class pixels-var x-var y-var))
(defgeneric image-alpha-set-code (image-class pixels-var x-var y-var
                                 gray-var))
(defgeneric image-alpha-blend-code (image-class pixels-var x-var y-var
                                    alpha-var))
;; rgba
(deftype image-rgba-get-fn () '(function (fixnum fixnum) (values octet octet octet octet)))
(deftype image-rgba-set-fn () '(function (fixnum fixnum octet octet octet octet)))
(deftype image-rgba-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
(deftype image-gray-alpha-get-fn () '(function (fixnum fixnum) (values octet octet)))
;; rgb
(deftype image-rgb-get-fn () '(function (fixnum fixnum) (values octet octet octet)))
(deftype image-rgb-set-fn () '(function (fixnum fixnum octet octet octet)))
(deftype image-rgb-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
;; gray
(deftype image-gray-get-fn () '(function (fixnum fixnum) octet))
(deftype image-gray-set-fn () '(function (fixnum fixnum octet)))
(deftype image-gray-blend-fn () '(function (fixnum fixnum octet octet)))
;; alpha
(deftype image-alpha-get-fn () '(function (fixnum fixnum) octet))
(deftype image-alpha-set-fn () '(function (fixnum fixnum octet )))
(deftype image-alpha-blend-fn () '(function (fixnum fixnum octet octet)))

;; rgba
(defgeneric image-rgba-get-fn (image &key dx dy region))
(defgeneric image-rgba-set-fn (image &key dx dy))
(defgeneric image-rgba-blend-fn (image &key dx dy))
(defgeneric image-gray-alpha-get-fn (image &key dx dy region))
;; rgb
(defgeneric image-rgb-get-fn (image &key dx dy region))
(defgeneric image-rgb-set-fn (image &key dx dy))
(defgeneric image-rgb-blend-fn (image &key dx dy))
;; gray
(defgeneric image-gray-get-fn (image &key dx dy region))
(defgeneric image-gray-set-fn (image &key dx dy))
(defgeneric image-gray-blend-fn (image &key dx dy))
;; alpha
(defgeneric image-alpha-get-fn (image &key dx dy region))
(defgeneric image-alpha-set-fn (image &key dx dy))
(defgeneric image-alpha-blend-fn (image &key dx dy))

;;;
;;; image I/O
;;;

(defgeneric read-image (source &key format))
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

(defmethod read-image (pathname &key format)
  (unless format
    (setf format (intern (string-upcase
                          (pathname-type (pathname pathname)))
                         (find-package :keyword))))
  (if (image-format-read-supported-p format)
      (funcall (gethash format *image-file-readers*)
               pathname)
      (error "image format not supproted, yet")))

(defmacro define-image-file-writer (format (&rest args) &body body)
  `(setf (gethash ,format *image-file-writer*)
         #'(lambda (,@args)
             ,@body)))

(defun image-format-write-supported-p (format)
  "Return true if FORMAT is supported by `read-image'."
  (not (null (gethash format *image-file-writer*))))

(defmethod write-image (image destination &key format quality)
  (declare (ignorable quality))
  (unless format
    (setf format (intern (string-upcase
                          (pathname-type (pathname destination)))
                         (find-package :keyword))))
  (if (image-format-write-supported-p format)
      (funcall (gethash format *image-file-writer*)
               image destination)
      (error "image format not supproted, yet")))

;;;
;;; Image operations
;;;

(defgeneric coerce-image (image image-class)
  (:method :around ((image image-mixin) image-class)
           (if (keywordp image-class)
               (call-next-method image (find-image-class image image-class))
               (call-next-method))))

(defgeneric clone-image (image image-class)
  (:method :around ((image image-mixin) image-class)
           (if (keywordp image-class)
               (call-next-method image (find-image-class image image-class))
               (call-next-method))))

(defgeneric copy-image (src-image sx sy width height dst-image x y))
(defgeneric blend-image (src-image sx sy width height dst-image x y &key alpha))

(defgeneric coerce-alpha-channel (image &optional image-class)
  (:method :around ((image image-mixin) &optional image-class)
           (if image-class
               (call-next-method)
               (call-next-method image (find-image-class image :gray)))))

(defgeneric clone-alpha-channel (image &optional image-class)
  (:method :around ((image image-mixin) &optional image-class)
           (if image-class
               (call-next-method)
               (call-next-method image (find-image-class image :gray)))))

(defgeneric copy-alpha-channel (src-image sx sy width height dst-image x y))
