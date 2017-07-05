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
  (mk-rgb-image-primitives opticl-rgb-image opticl-rgb-image-pixels
                           pixels-var x-var y-var red-var green-var blue-var
                           `(multiple-value-bind (r g b)
                                (opticl:pixel ,pixels-var ,y-var ,x-var)
                              (values r g b 255))
                           `(setf (opticl:pixel ,pixels-var ,y-var ,x-var)
                                  (values ,red-var ,green-var ,blue-var))))

(mk-image-functions opticl-rgb-image)
(mk-rgb-image-functions opticl-rgb-image)
(mk-fast-copy-image opticl-rgb-image opticl-rgb-image)

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
  (mk-rgba-image-primitives opticl-rgba-image opticl-rgba-image-pixels
                            pixels-var x-var y-var red-var green-var blue-var alpha-var
                            `(opticl:pixel ,pixels-var ,y-var ,x-var)
                            `(setf (opticl:pixel ,pixels-var ,y-var ,x-var)
                                   (values ,red-var ,green-var ,blue-var ,alpha-var))))

(mk-image-functions opticl-rgba-image)
(mk-rgba-image-functions opticl-rgba-image)
(mk-fast-copy-image opticl-rgba-image opticl-rgba-image)

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
