(in-package :mcclim-render)



;;;
;;; Opticl RGB
;;;

;;; Opticl Stencil

;;;
(deftype opticl-stencil-image-data () 'opticl-core:8-bit-gray-image)

(defclass opticl-stencil-image (opticl-image stencil-image-mixin)
  ((clim-image::pixels :type (or null opticl-stencil-image-data))))

(defun make-opticl-stencil-image (width height)
  (let ((data (opticl:make-8-bit-gray-image height width :initial-element 0)))
    (make-instance 'opticl-stencil-image
		   :width width
		   :height height
		   :pixels data)))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defmethod image-pixels-type ((image-class (eql 'opticl-stencil-image)))
    'opticl-stencil-image-data)

  (defmethod make-get-alpha-octet-code ((image-class (eql 'opticl-stencil-image)) pixels-var x-var y-var)
    `(the octet (opticl:pixel ,pixels-var ,y-var ,x-var)))
  (defmethod make-set-alpha-octet-code ((image-class (eql 'opticl-stencil-image)) pixels-var x-var y-var alpha-var)
    `(setf (opticl:pixel ,pixels-var ,y-var ,x-var) ,alpha-var)))


;;;
;;; Pixeled Design
;;;

(defmethod  make-pixeled-image-rgba-octets-fn ((image opticl-rgb-image) dx dy region)
  (let ((data (image-pixels image)))
    (declare (type opticl-rgb-image-pixels data))
    (if (image-alpha-p image)
        (lambda (x y)
          (declare (type fixnum x y))
          (if (clim:region-contains-position-p region x y)
              (opticl:pixel data (+ y dy) (+ x dx))
              (values 0 0 0 0)))
        (lambda (x y)
          (declare (type fixnum x y))
          (if (clim:region-contains-position-p region x y)
              (multiple-value-bind (red green blue)
                  (opticl:pixel data (+ y dy) (+ x dx))
                (values red
                        green
                        blue
                        255))
              (values 0 0 0 0))))))

(defmethod  make-pixeled-image-rgba-octets-unsafe-fn ((image opticl-rgb-image) dx dy region)
  (let ((data (image-pixels image)))
    (declare (type opticl-rgb-image-pixels data))
    (if (image-alpha-p image)
        (lambda (x y)
          (declare (type fixnum x y))
          (opticl:pixel data (+ y dy) (+ x dx)))
        (lambda (x y)
          (declare (type fixnum x y))
          (multiple-value-bind (red green blue)
              (opticl:pixel data (+ y dy) (+ x dx))
            (values red
                    green
                    blue
                    255))))))

(defmethod  make-pixeled-image-rgba-octets-fn ((image opticl-stencil-image) dx dy region)
  (let ((data (image-pixels image)))
    (declare (type opticl-stencil-image-data data))
    (lambda (x y)
      (declare (type fixnum x y))
      (if (clim:region-contains-position-p region x y)
          (let ((p (opticl:pixel data (+ y dy) (+ x dx))))
            (values 0 0 0 p))
          (values 0 0 0 0)))))

(defmethod  make-pixeled-image-rgba-octets-unsafe-fn ((image opticl-stencil-image) dx dy region)
  (let ((data (image-pixels image)))
    (declare (type opticl-stencil-image-data data))
    (lambda (x y)
      (declare (type fixnum x y))
      (let ((p (opticl:pixel data (+ y dy) (+ x dx))))
        (values 0 0 0 p)))))

;;;
;;; Operations
;;;

;;(make-copy-image opticl-rgb-image opticl-rgb-image)
;;(make-copy-image opticl-rgb-image rgb-image)
;;(make-copy-image rgb-image opticl-rgb-image)
(make-fill-image-with-stencil opticl-rgb-image opticl-stencil-image)
(make-fill-image-without-stencil opticl-rgb-image)

(make-make-aa-render-draw-fn opticl-rgb-image)
(make-make-aa-render-draw-span-fn opticl-rgb-image)
(make-make-aa-render-xor-draw-fn opticl-rgb-image)
(make-make-aa-render-xor-draw-span-fn opticl-rgb-image)
(make-make-aa-render-alpha-draw-fn opticl-stencil-image)
(make-make-aa-render-alpha-draw-span-fn opticl-stencil-image)

;;;
;;; I/O
;;;

