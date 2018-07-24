(in-package :mcclim-render-internals)

;;(declaim (optimize speed))

;;;
;;; Pixel - RGB translators
;;;

(defun mask->byte (mask)
  (let ((h (integer-length mask)))
    (let ((l (integer-length (logxor mask (1- (ash 1 h))))))
      (byte (- h l) l))))

(defvar *translator-cache-lock* (clim-sys:make-lock "translator cache lock"))
(defparameter *rgb->pixel-translator-cache* (make-hash-table :test #'equal))
(defparameter *pixel->rgb-translator-cache* (make-hash-table :test #'equal))

(defun rgb->pixel-translator (colormap)
  (unless (eq (xlib:visual-info-class (xlib:colormap-visual-info colormap))
	      :true-color)
    (error "sorry, cannot draw rgb image for non-true-color drawable yet"))
  (let* ((info (xlib:colormap-visual-info colormap))
	 (rbyte (mask->byte (xlib:visual-info-red-mask info)))
	 (gbyte (mask->byte (xlib:visual-info-green-mask info)))
	 (bbyte (mask->byte (xlib:visual-info-blue-mask info)))
         (abyte (mask->byte (- #xFFFFFFFF
                               (+ (xlib:visual-info-red-mask info)
                                  (xlib:visual-info-green-mask info)
                                  (xlib:visual-info-blue-mask info)))))
         (key (list rbyte gbyte bbyte)))
    (clim-sys:with-lock-held (*translator-cache-lock*)
      (or (gethash key *rgb->pixel-translator-cache*)
          (setf (gethash key *rgb->pixel-translator-cache*)
                (compile nil
                         `(lambda (r g b a)
                            (dpb r ',rbyte
                                 (dpb g ',gbyte
                                      (dpb b ',bbyte
                                           (dpb a ',abyte
                                                0)))))))))))

(defun pixel->rgb-translator (colormap)
  (unless (eq (xlib:visual-info-class (xlib:colormap-visual-info colormap))
	      :true-color)
    (error "sorry, cannot draw rgb image for non-true-color drawable yet"))
  (let* ((info (xlib:colormap-visual-info colormap))
	 (rbyte (mask->byte (xlib:visual-info-red-mask info)))
	 (gbyte (mask->byte (xlib:visual-info-green-mask info)))
	 (bbyte (mask->byte (xlib:visual-info-blue-mask info)))
         (abyte (mask->byte (- #xFFFFFFFF
                               (+ (xlib:visual-info-red-mask info)
                                  (xlib:visual-info-green-mask info)
                                  (xlib:visual-info-blue-mask info)))))
         (key (list rbyte gbyte bbyte)))
    (clim-sys:with-lock-held (*translator-cache-lock*)
      (or (gethash key *pixel->rgb-translator-cache*)
          (setf (gethash key *pixel->rgb-translator-cache*)
                (compile nil
                         `(lambda (p)
                            (values (ldb ',rbyte p)
                                    (ldb ',gbyte p)
                                    (ldb ',bbyte p)
                                    (ldb ',abyte p)))))))))

;;;
;;; Clx images
;;;

(defclass clx-image (basic-image)
  ())

;;;
;;; RGB
;;;
(deftype clx-basic-image-pixels () '(simple-array (unsigned-byte 32) (* *)))

(defclass clx-basic-image (clx-image rgb-image-mixin)
  ((medium :initarg :medium :reader image-medium)
   (colormap :initarg :colormap)
   (pixels :type clx-rgb-image-pixels)))

(defmethod initialize-instance :after ((image clx-basic-image)
                                       &key)
  (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (make-array (list height width)
                        :element-type '(unsigned-byte 32)
                        :initial-element #xFFFFFFFF)))))

(defclass clx-rgba-image (clx-basic-image rgba-image-mixin)
  ())

(defmethod image-rgba-get-fn ((image clx-rgba-image) &key (dx 0) (dy 0) (region nil))
  (with-slots (colormap) image
    (let ((pixels (image-pixels image))
          (translator (pixel->rgb-translator colormap)))
      (declare (type clx-basic-image-pixels pixels)
               (type fixnum dx dy)
               (type (function (fixnum) (values octet octet octet octet)) translator))
      (lambda (x y)
        (declare (type fixnum x y))
        (if (or (not region) (clim:region-contains-position-p region x y))
            (let ((p (aref pixels (+ y dy) (+ x dx))))
              (multiple-value-bind (r g b a)
                  (funcall translator p)
                (values r g b a)))
            (values 0 0 0 0))))))

(defmethod image-rgba-set-fn ((image clx-rgba-image) &key (dx 0) (dy 0))
  (with-slots (colormap) image
    (let ((pixels (image-pixels image))
          (translator (rgb->pixel-translator colormap)))
      (declare (type clx-basic-image-pixels pixels)
               (type fixnum dx dy)
               (type (function (octet octet octet octet) fixnum) translator))
      (lambda (x y red green blue alpha)
        (declare (type fixnum x y)
                 (type octet red green blue))
        (setf (aref pixels (+ y dy) (+ x dx))
              (funcall translator red green blue alpha))))))

(defclass clx-rgb-image (clx-basic-image rgb-image-mixin)
  ())

(defmethod image-rgb-get-fn ((image clx-rgb-image) &key (dx 0) (dy 0) (region nil))
  (with-slots (colormap) image
    (let ((pixels (image-pixels image))
          (translator (pixel->rgb-translator colormap)))
      (declare (type clx-basic-image-pixels pixels)
               (type fixnum dx dy)
               (type (function (fixnum) (values octet octet octet octet)) translator))
      (lambda (x y)
        (declare (type fixnum x y))
        (if (or (not region) (clim:region-contains-position-p region x y))
            (let ((p (aref pixels (+ y dy) (+ x dx))))
              (multiple-value-bind (r g b a)
                  (funcall translator p)
                (rgba->rgb r g b a)))
            (values 0 0 0))))))

(defmethod image-rgb-set-fn ((image clx-rgb-image) &key (dx 0) (dy 0))
  (with-slots (colormap) image
    (let ((pixels (image-pixels image))
          (translator (rgb->pixel-translator colormap)))
      (declare (type clx-basic-image-pixels pixels)
               (type fixnum dx dy)
               (type (function (octet octet octet octet) fixnum) translator))
      (lambda (x y red green blue)
        (declare (type fixnum x y)
                 (type octet red green blue))
        (multiple-value-bind (r g b a)
            (rgb->rgba red green blue)
          (setf (aref pixels (+ y dy) (+ x dx))
                (funcall translator r g b a)))))))

(defclass clx-gray-image (clx-basic-image gray-image-mixin)
  ())

(defmethod image-gray-get-fn ((image clx-gray-image) &key (dx 0) (dy 0) (region nil))
  (with-slots (colormap) image
    (let ((pixels (image-pixels image))
          (translator (pixel->rgb-translator colormap)))
      (declare (type clx-basic-image-pixels pixels)
               (type fixnum dx dy)
               (type (function (fixnum) (values octet octet octet octet)) translator))
      (lambda (x y)
        (declare (type fixnum x y))
        (if (or (not region) (clim:region-contains-position-p region x y))
            (let ((p (aref pixels (+ y dy) (+ x dx))))
              (multiple-value-bind (r g b a)
                  (funcall translator p)
                (rgba->gray r g b a)))
            0)))))

(defmethod image-gray-set-fn ((image clx-gray-image) &key (dx 0) (dy 0))
  (with-slots (colormap) image
    (let ((pixels (image-pixels image))
          (translator (rgb->pixel-translator colormap)))
      (declare (type clx-basic-image-pixels pixels)
               (type fixnum dx dy)
               (type (function (octet octet octet octet) fixnum) translator))
      (lambda (x y gray)
        (declare (type fixnum x y)
                 (type octet gray))
        (multiple-value-bind (r g b a)
            (gray->rgba gray)
          (setf (aref pixels (+ y dy) (+ x dx))
                (funcall translator r g b a)))))))

(defun medium-colormap (medium)
  (xlib:window-colormap (clim-clx::sheet-xmirror (medium-sheet medium))))

(defmethod make-image ((medium clim-clx::clx-medium) (type (eql :rgba)) width height)
  (make-instance 'clx-rgba-image :width width :height height
                 :medium medium
                 :colormap (medium-colormap medium)))

(defmethod make-image ((medium clim-clx::clx-medium) (type (eql :rgb)) width height)
  (make-instance 'clx-rgb-image :width width :height height
                 :medium medium
                 :colormap (medium-colormap medium)))

(defmethod make-image ((medium clim-clx::clx-medium) (type (eql :gray)) width height)
  (make-instance 'clx-gray-image :width width :height height
                 :medium medium
                 :colormap (medium-colormap medium)))

(defmethod make-image ((medium clim-clx::clx-medium) (type (eql :auto)) width height)
  (make-instance 'clx-rgb-image :width width :height height
                 :medium medium
                 :colormap (medium-colormap medium)))

