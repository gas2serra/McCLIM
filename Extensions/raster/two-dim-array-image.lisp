(in-package :mcclim-raster-internals)

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
  (def-rgb-image-primitives rgb-image rgb-image-pixels
                           pixels-var x-var y-var red-var green-var blue-var alpha-var
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

(def-rgb-image-functions rgb-image)

;;;
;;; RGBA
;;;
(deftype rgba-image-pixels () '(simple-array (unsigned-byte 32) (* *)))

(defclass rgba-image (two-dim-array-image drawable-image rgba-image-mixin)
  ((pixels :type rgba-image-pixels)))

(defmethod initialize-instance :after ((image rgba-image)
                                       &key)
  (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (make-array (list height width)
                        :element-type '(unsigned-byte 32)
                        :initial-element #xFFFFFFFF)))))

(defun make-rgba-image (width height)
  (let ((data (make-array (list height width)
                          :element-type '(unsigned-byte 32)
                          :initial-element #xFFFFFFFF)))
    (make-instance 'rgba-image
		   :width width
		   :height height
		   :pixels data)))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (def-rgba-image-primitives rgba-image rgba-image-pixels
                            pixels-var x-var y-var red-var green-var blue-var alpha-var
                            `(let ((p (aref ,pixels-var
                                            ,y-var
                                            ,x-var)))
                               (values (ldb (byte 8 0) p)
                                       (ldb (byte 8 8) p)
                                       (ldb (byte 8 16) p)
                                       (ldb (byte 8 24) p)))
                            `(setf (aref ,pixels-var ,y-var ,x-var)
                                   (dpb ,red-var (byte 8 0)
                                        (dpb ,green-var (byte 8 8)
                                             (dpb ,blue-var (byte 8 16)
                                                  (dpb ,alpha-var (byte 8 24) 0)))))
                            `(setf (aref ,pixels-var ,y-var ,x-var)
                                   (dpb ,alpha-var (byte 8 24)
                                        (aref ,pixels-var ,y-var ,x-var)))))


(def-rgba-image-functions rgba-image)

;;;
;;; Single channel
;;;
(deftype single-channel-image-pixels () '(simple-array (unsigned-byte 8) (* *)))

(defclass single-channel-image (two-dim-array-image)
  ((pixels :type single-channel-image-pixels)))

(defmethod initialize-instance :after ((image single-channel-image)
                                       &key)
  (let ((width (image-width image))
        (height (image-height image)))
    (when (and width height (not (slot-boundp image 'pixels)))
      (setf (slot-value image 'pixels)
            (make-array (list height width)
                        :element-type '(unsigned-byte 8)
                        :initial-element #x00)))))

;;;
;;; Gray
;;;
(defclass gray-image (single-channel-image drawable-image gray-image-mixin)
  ())

(defun make-gray-image (width height)
  (make-instance 'gray-image
                 :width width
                 :height height))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (def-gray-image-primitives gray-image single-channel-image-pixels
                            pixels-var x-var y-var gray-var alpha-var
                            `(aref ,pixels-var ,y-var ,x-var)
                            `(setf (aref ,pixels-var ,y-var ,x-var) ,gray-var)
                            `(setf (aref ,pixels-var ,y-var ,x-var) ,alpha-var)))

(def-gray-image-functions gray-image)

;;;
;;; Configuration & Optimization
;;;

(defmethod find-image-class ((image two-dim-array-image) (type (eql :rgba)))
  'rgba-image)

(defmethod find-image-class ((image two-dim-array-image) (type (eql :rgb)))
  'rgb-image)

(defmethod find-image-class ((image two-dim-array-image) (type (eql :gray)))
  'gray-image)

(def-fast-rgb-copy-image rgb-image rgb-image)
