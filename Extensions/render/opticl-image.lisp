(in-package :mcclim-render)



;;;
;;; Opticl RGB
;;;

;;; Opticl Stencil

;;;
;;(deftype opticl-stencil-image-data () 'opticl-core:8-bit-gray-image)

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
(make-fill-image-with-stencil opticl-rgb-image opticl-gray-image)
(make-fill-image-without-stencil opticl-rgb-image)

(make-make-aa-render-draw-fn opticl-rgb-image)
(make-make-aa-render-draw-span-fn opticl-rgb-image)
(make-make-aa-render-xor-draw-fn opticl-rgb-image)
(make-make-aa-render-xor-draw-span-fn opticl-rgb-image)
(make-make-aa-render-alpha-draw-fn opticl-gray-image)
(make-make-aa-render-alpha-draw-span-fn opticl-gray-image)

;;;
;;; I/O
;;;

