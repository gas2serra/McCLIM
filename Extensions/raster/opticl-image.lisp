(in-package :mcclim-raster)

;;;
;;; Opticl
;;;
(defclass opticl-image (basic-image)
  ())

;;;
;;; RGB
;;;
(deftype opticl-rgb-image-pixels () 'opticl-core:8-bit-rgb-image)

(defclass opticl-rgb-image (opticl-image drawable-image rgb-image-mixin)
  ((pixels :type opticl-rgb-image-pixels)))

(defmethod initialize-instance :after ((image opticl-rgb-image)
                                       &key)
  (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (opticl:make-8-bit-rgb-image height width :initial-element 255)))))

(defun make-opticl-rgb-image (width height)
  (make-instance 'opticl-rgb-image
                 :width width
                 :height height))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defmethod image-pixels-type ((image-class (eql 'opticl-rgb-image)))
    'opticl-rgb-image-pixels)

  (defmethod make-get-rgb-octets-code ((image-class (eql 'opticl-rgb-image)) pixels-var x-var y-var)
    `(opticl:pixel ,pixels-var ,y-var ,x-var))

  (defmethod make-set-rgb-octets-code ((image-class (eql 'opticl-rgb-image)) pixels-var x-var y-var red-var green-var blue-var)
    `(setf (opticl:pixel ,pixels-var ,y-var ,x-var)
           (values ,red-var ,green-var ,blue-var)))

  (defmethod make-get-rgba-octets-code ((image-class (eql 'opticl-rgb-image)) pixels-var x-var y-var)
    `(multiple-value-bind (r g b a)
         (opticl:pixel ,pixels-var ,y-var ,x-var)
       (values r g b 255)))

  (defmethod make-set-rgba-octets-code ((image-class (eql 'opticl-rgb-image)) pixels-var x-var y-var
                                       red-var green-var blue-var alpha-var)
    `(setf (opticl:pixel ,pixels-var ,y-var ,x-var)
           (values ,red-var ,green-var ,blue-var 255))))

(make-map-rgb-color opticl-rgb-image)
(make-copy-image opticl-rgb-image opticl-rgb-image)
(make-copy-image opticl-rgb-image rgb-image)
(make-copy-image rgb-image opticl-rgb-image)

;;;
;;; RGBA
;;;

(deftype opticl-rgba-image-pixels () 'opticl-core:8-bit-rgba-image)

(defclass opticl-rgba-image (opticl-image drawable-image rgb-image-mixin)
  ((pixels :type opticl-rgba-image-pixels)))

(defmethod initialize-instance :after ((image opticl-rgba-image)
                                       &key)
  (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (opticl:make-8-bit-rgba-image height width :initial-element 255)))))

(defun make-opticl-rgba-image (width height)
  (make-instance 'opticl-rgba-image
                 :width width
                 :height height))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defmethod image-pixels-type ((image-class (eql 'opticl-rgba-image)))
    'opticl-rgba-image-pixels)

  (defmethod make-get-rgb-octets-code ((image-class (eql 'opticl-rgba-image)) pixels-var x-var y-var)
    `(multiple-value-bind (r g b a)
         (opticl:pixel ,pixels-var ,y-var ,x-var)
       (values r g b)))
  (defmethod make-set-rgb-octets-code ((image-class (eql 'opticl-rgba-image)) pixels-var x-var y-var red-var green-var blue-var)
    `(setf (opticl:pixel ,pixels-var ,y-var ,x-var)
           (values ,red-var ,green-var ,blue-var 255)))

  (defmethod make-get-rgba-octets-code ((image-class (eql 'opticl-rgba-image)) pixels-var x-var y-var)
    `(opticl:pixel ,pixels-var ,y-var ,x-var))
  (defmethod make-set-rgba-octets-code ((image-class (eql 'opticl-rgba-image)) pixels-var x-var y-var
                                       red-var green-var blue-var alpha-var)
    `(setf (opticl:pixel ,pixels-var ,y-var ,x-var)
           (values ,red-var ,green-var ,blue-var ,alpha-var))))

(make-map-rgb-color opticl-rgba-image)
(make-copy-image opticl-rgba-image opticl-rgba-image)
(make-copy-image opticl-rgba-image opticl-rgb-image)
(make-copy-image opticl-rgba-image rgb-image)

;;;
;;; I/O
;;;
(defmacro define-opticl-image-file-reader (format)
  `(define-image-file-reader ,format (pathname)
     (let ((pixels (opticl:read-image-file pathname)))
       (opticl:with-image-bounds (height width channels)
           pixels
         (cond ((not channels)
                (make-instance 'opticl-rgba-image
                               :width width
                               :height height
                               :pixels (if (typep pixels 'opticl:8-bit-rgba-image)
                                           pixels
                                           (opticl:coerce-image pixels 'opticl:8-bit-rgba-image))))
               ((= channels 4)
                (make-instance 'opticl-rgba-image
                               :width width
                               :height height
                               :pixels (if (typep pixels 'opticl:8-bit-rgba-image)
                                           pixels
                                           (opticl:coerce-image pixels 'opticl:8-bit-rgba-image))))
               (t
                (make-instance 'opticl-rgb-image
                               :width width
                               :height height
                               :pixels (if (typep pixels 'opticl:8-bit-rgb-image)
                                           pixels
                                           (opticl:coerce-image pixels 'opticl:8-bit-rgb-image)))))))))

(define-opticl-image-file-reader :tiff)
(define-opticl-image-file-reader :tif)
(define-opticl-image-file-reader :jpeg)
(define-opticl-image-file-reader :jpg)
(define-opticl-image-file-reader :png)
(define-opticl-image-file-reader :pbm)
(define-opticl-image-file-reader :pgm)
(define-opticl-image-file-reader :ppm)
(define-opticl-image-file-reader :gif)

(defmacro define-opticl-image-file-writer (format fn)
  `(define-image-file-writer ,format (image destination)
     (declare (ignore quality))
     (let ((img (coerce-image image 'opticl-rgb-image)))
       (if ,fn
           (if (streamp destination)
               (funcall ,fn destination (image-pixels img))
               (opticl:write-image-file destination (image-pixels img)))
           (error "Cannot write image to: ~S" destination)))))

(define-opticl-image-file-writer :tiff #'opticl:write-tiff-stream)
(define-opticl-image-file-writer :tif #'opticl:write-tiff-stream)
(define-opticl-image-file-writer :jpeg #'opticl:write-jpeg-stream)
(define-opticl-image-file-writer :jpg #'opticl:write-jpeg-stream)
(define-opticl-image-file-writer :png #'opticl:write-png-stream)
(define-opticl-image-file-writer :pbm #'opticl:write-pbm-stream)
(define-opticl-image-file-writer :pgm #'opticl:write-pgm-stream)
(define-opticl-image-file-writer :gif #'opticl:write-gif-stream)
