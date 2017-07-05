(in-package :mcclim-raster)

;;;
;;; Two dimensional array of pixels
;;;

(defclass two-dim-array-image (basic-image)
  ())

;;;
;;; RGB
;;;
(deftype rgb-image-pixels () '(simple-array (unsigned-byte 32) (* *)))

(defclass rgb-image (two-dim-array-image drawable-image rgb-image-mixin)
  ((pixels :type rgb-image-pixels)))

(defmethod initialize-instance :after ((image rgb-image)
                                       &key)
  (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (make-array (list height width)
                        :element-type '(unsigned-byte 32)
                        :initial-element #xFFFFFFFF)))))

(defun make-rgb-image (width height)
  (let ((data (make-array (list height width)
                          :element-type '(unsigned-byte 32)
                          :initial-element #xFFFFFFFF)))
    (make-instance 'rgb-image
		   :width width
		   :height height
		   :pixels data)))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (mk-rgb-image-primitives rgb-image rgb-image-pixels
                           pixels-var x-var y-var red-var green-var blue-var
                           `(let ((p (aref ,pixels-var
                                           ,y-var
                                           ,x-var)))
                             (values (ldb (byte 8 0) p)
                                     (ldb (byte 8 8) p)
                                     (ldb (byte 8 16) p)
                                     255))
                           `(setf (aref ,pixels-var ,y-var ,x-var)
                                  (dpb ,red-var (byte 8 0)
                                       (dpb ,green-var (byte 8 8)
                                            (dpb ,blue-var (byte 8 16)
                                                 (dpb 255 (byte 8 24) 0)))))))

(mk-image-functions rgb-image)
(mk-rgb-image-functions rgb-image)
(mk-fast-copy-image rgb-image rgb-image)
