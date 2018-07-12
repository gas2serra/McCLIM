(in-package :mcclim-render-internals)

;;;
;;; Opticl
;;;
(defclass opticl-image (basic-image)
  ())

(defmethod image-family ((image  opticl-image))
  :opticl)

;;;
;;; RGBA
;;;
(deftype opticl-rgba-image-pixels () 'opticl-core:8-bit-rgba-image)

(defclass opticl-rgba-image (opticl-image drawable-image rgba-image-mixin)
  ((pixels :type opticl-rgba-image-pixels)))

(defmethod initialize-instance :after ((image opticl-rgba-image)
                                       &key)
  (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (opticl:make-8-bit-rgba-image height width :initial-element 255)))))

(defmethod image-rgba-get-fn ((image  opticl-rgba-image) &key (dx 0) (dy 0) (region nil))
  (let ((pixels (image-pixels image)))
    (declare (type  opticl-rgba-image-pixels pixels)
             (type fixnum dx dy))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (or (not region) (clim:region-contains-position-p region x y))
          (opticl:pixel pixels (+ y dy) (+ x dx))
          (values 0 0 0 0)))))

(defmethod image-rgba-set-fn ((image  opticl-rgba-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image)))
    (declare (type opticl-rgba-image-pixels pixels)
             (type fixnum dx dy))
    (lambda (x y red green blue alpha)
      (declare (type fixnum x y))
      (setf (opticl:pixel pixels (+ y dy) (+ x dx))
            (values red green blue alpha)))))

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

(defmethod image-rgb-get-fn ((image  opticl-rgb-image) &key (dx 0) (dy 0) (region nil))
  (let ((pixels (image-pixels image)))
    (declare (type opticl-rgb-image-pixels pixels)
             (type fixnum dx dy))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (or (not region) (clim:region-contains-position-p region x y))
          (opticl:pixel pixels (+ y dy) (+ x dx))
          (values 0 0 0)))))

(defmethod image-rgb-set-fn ((image  opticl-rgb-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image)))
    (declare (type opticl-rgb-image-pixels pixels)
             (type fixnum dx dy))
    (lambda (x y red green blue)
      (declare (type fixnum x y))
      (setf (opticl:pixel pixels (+ y dy) (+ x dx))
            (values red green blue)))))

;;;
;;; Gray
;;;
(deftype opticl-gray-image-pixels () 'opticl-core:8-bit-gray-image)

(defclass opticl-gray-image (opticl-image drawable-image gray-image-mixin)
  ((pixels :type opticl-gray-image-pixels)))

(defmethod initialize-instance :after ((image opticl-gray-image)
                                       &key)
  (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (opticl:make-8-bit-gray-image height width :initial-element 0)))))

(defmethod image-gray-get-fn ((image  opticl-gray-image) &key (dx 0) (dy 0) (region nil))
  (let ((pixels (image-pixels image)))
    (declare (type  opticl-gray-image-pixels pixels)
             (type fixnum dx dy))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (or (not region) (clim:region-contains-position-p region x y))
          (opticl:pixel pixels (+ y dy) (+ x dx))
          0))))

(defmethod image-gray-set-fn ((image opticl-gray-image) &key (dx 0) (dy 0))
  (let ((pixels (image-pixels image)))
    (declare (type opticl-gray-image-pixels pixels)
             (type fixnum dx dy))
    (lambda (x y gray)
      (declare (type fixnum x y gray))
      (setf (opticl:pixel pixels y x)
            gray))))

;;;
;;; Configuration & Optimization
;;;
(defmethod find-image-class ((family (eql :opticl)) (type (eql :rgba)))
  'opticl-rgba-image)

(defmethod find-image-class ((family (eql :opticl)) (type (eql :rgb)))
  'opticl-rgb-image)

(defmethod find-image-class ((family (eql :opticl)) (type (eql :gray)))
  'opticl-gray-image)

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
               ((= channels 1)
                (make-instance 'opticl-gray-image
                               :width width
                               :height height
                               :pixels (if (typep pixels 'opticl:8-bit-gray-image)
                                           pixels
                                           (opticl:coerce-image pixels 'opticl:8-bit-gray-image))))
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
