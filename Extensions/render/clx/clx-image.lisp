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
         (key (list rbyte gbyte bbyte)))
    (clim-sys:with-lock-held (*translator-cache-lock*)
      (or (gethash key *rgb->pixel-translator-cache*)
          (setf (gethash key *rgb->pixel-translator-cache*)
                (compile nil
                         `(lambda (r g b)
                            (declare (ignore x y))
                            (dpb r ',rbyte
                                 (dpb g ',gbyte
                                      (dpb b ',bbyte
                                           0))))))))))

(defun pixel->rgb-translator (colormap)
  (unless (eq (xlib:visual-info-class (xlib:colormap-visual-info colormap))
	      :true-color)
    (error "sorry, cannot draw rgb image for non-true-color drawable yet"))
  (let* ((info (xlib:colormap-visual-info colormap))
	 (rbyte (mask->byte (xlib:visual-info-red-mask info)))
	 (gbyte (mask->byte (xlib:visual-info-green-mask info)))
	 (bbyte (mask->byte (xlib:visual-info-blue-mask info)))
         (key (list rbyte gbyte bbyte)))
    (clim-sys:with-lock-held (*translator-cache-lock*)
      (or (gethash key *pixel->rgb-translator-cache*)
          (setf (gethash key *pixel->rgb-translator-cache*)
                (compile nil
                         `(lambda (p)
                            (values (ldb ',rbyte p)
                                    (ldb ',gbyte p)
                                    (ldb ',bbyte p)))))))))

;;;
;;; Clx images
;;;

(defclass clx-image (basic-image)
  ())

(defmethod image-family ((image clx-image))
  :clx)

;;;
;;; RGB
;;;
(deftype clx-rgb-image-pixels () '(simple-array (unsigned-byte 32) (* *)))

(defclass clx-rgb-image (clx-image rgb-image-mixin)
  ((colormap :initarg :colormap)
   (pixels :type clx-rgb-image-pixels)))

(defmethod initialize-instance :after ((image clx-rgb-image)
                                       &key)
  (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (make-array (list height width)
                        :element-type '(unsigned-byte 32)
                        :initial-element #xFFFFFFFF)))))

(defmethod image-rgb-get-fn ((image clx-rgb-image) &key (dx 0) (dy 0) (region nil))
  (with-slots (colormap) image
    (let ((pixels (image-pixels image))
          (translator (pixel->rgb-translator colormap)))
      (declare (type clx-rgb-image-pixels pixels)
               (type fixnum dx dy)
               (type (function (fixnum) (values octet octet octet)) translator))
      (lambda (x y)
        (declare (type fixnum x y))
        (if (or (not region) (clim:region-contains-position-p region x y))
            (let ((p (aref pixels (+ y dy) (+ x dx))))
              (multiple-value-bind (r g b)
                  (funcall translator p)
                (values r g b)))
            (values 0 0 0))))))

(defmethod image-rgb-set-fn ((image clx-rgb-image) &key (dx 0) (dy 0))
  (with-slots (colormap) image
    (let ((pixels (image-pixels image))
          (translator  (rgb->pixel-translator colormap)))
      (declare (type clx-rgb-image-pixels pixels)
               (type fixnum dx dy)
               (type (function (octet octet octet) fixnum) translator))
      (lambda (x y red green blue)
        (declare (type fixnum x y)
                 (type octet red green blue))
        (setf (aref pixels (+ y dy) (+ x dx))
              (funcall translator red green blue))))))
