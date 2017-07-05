(in-package :mcclim-raster)

;;;
;;; Image
;;;
(defclass image ()
  ((width :initform 0 :initarg :width :accessor image-width :type fixnum)
   (height :initform 0 :initarg :height :accessor image-height :type fixnum)))

(defclass rgb-image-mixin ()
  ())

(defclass rgba-image-mixin ()
  ())

;;;
;;; Drawable Image
;;;
(defclass drawable-image (image)
  ())

(defgeneric map-rgb-color (drawable-image fn))

(defun draw-image* (medium image x y
                    &rest args
                    &key clipping-region transformation)
  (declare (ignorable clipping-region transformation args))
  (climi::with-medium-options (medium args)
    (medium-draw-image* medium image x y)))

(clim-internals::def-graphic-op draw-image* (image x y))

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

(defgeneric mage-pixels-type (image-class))
(defgeneric make-get-rgb-octets-code (image-class pixels-var x-var y-var))
(defgeneric make-set-rgb-octets-code (image-class pixels-var x-var y-var
                                      red-var grren-var blue-var))
(defgeneric make-get-rgba-octets-code (image-class pixels-var x-var y-var))
(defgeneric make-set-rgba-octets-code (image-class pixels-var x-var y-var
                                       red-var grren-var blue-var alpha-var))

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
  (unless format
    (setf format (intern (string-upcase
                          (pathname-type (pathname pathname)))
                         (find-package :keyword))))
  (if (image-format-write-supported-p format)
      (funcall (gethash format *image-file-writer*)
               image destination)
      (error "image format not supproted, yet")))

;;;
;;; Image operations
;;;

(defgeneric copy-image (src-image sx sy width height dst-image x y))
(defgeneric coerce-image (image image-class))
(defgeneric clone-image (image image-class))
