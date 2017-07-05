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

(defmethod  make-pixeled-image-rgba-octets-fn (image dx dy region)
  (image-rgb-get-fn image :dx dx :dy dy :region region))
(defmethod  make-pixeled-image-rgba-octets-unsafe-fn (image dx dy region)
  (image-rgb-get-fn image :dx dx :dy dy :region nil))

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

