

(defpackage :mcclim-raster
  (:nicknames #:clim-image)
  (:use)
  (:export
   ;; colors
   #:octet-rgba-blend-function
   #:octet-rgb-blend-function
   #:octet-gray-blend-function
   #:octet-alpha-blend-function
   #:color-value->octet
   #:color-octet->value
   #:color->octets
   ;; image
   #:image
   #:image-width
   #:image-height
   #:draw-image*
   #:medium-draw-image*
   #:image-design
   #:make-image-design
   #:image-pixels
   #:image-rgb-get-fn
   #:image-rgb-set-fn
   #:image-rgb-blend-fn
   #:image-rgba-get-fn
   #:image-rgba-set-fn
   #:image-rgba-blend-fn
   #:image-gray-get-fn
   #:image-gray-set-fn
   #:image-gray-blend-fn
   #:image-gray-alpha-get-fn
   #:image-alpha-get-fn
   #:image-alpha-set-fn
   #:image-alpha-blend-fn
   ;; image ops
   #:read-image
   #:write-image
   #:image-format-read-supported-p
   #:image-format-write-supported-p
   #:coerce-image
   #:clone-image
   #:copy-image
   #:blend-image
   ;; two dimensional array image
   #:two-dim-array-image
   #:rgb-image
   #:make-rgb-image
   #:rgba-image
   #:make-rgba-image
   #:gray-image
   #:make-gray-image
   #:stencil-image
   #:make-stencil-image
   ;; opticl image
   #:opticl-image
   #:opticl-rgb-image
   #:make-opticl-rgb-image
   #:opticl-rgba-image
   #:make-opticl-rgba-image
   #:opticl-gray-image
   #:make-opticl-gray-image
   #:opticl-stencil-image
   #:make-opticl-stencil-image
   ))

(defpackage :mcclim-raster-extensions
  (:use)
  (:export
   ;; colors
   #:octet
   #:color-octet-xor
   #:octet-mult
   #:octet-blend-function
   ;; image
   #:rgb-image-mixin
   #:rgba-image-mixin
   #:gray-image-mixin
   #:stencil-image-mixin
   #:drawable-image
   #:map-rgb-color
   #:basic-image
   #:image-pixels-type
   #:image-rgb-get-code
   #:image-rgb-set-code
   #:image-rgb-blend-code
   #:image-rgba-get-code
   #:image-rgba-set-code
   #:image-rgba-blend-code
   #:image-gray-get-code
   #:image-gray-set-code
   #:image-gray-blend-code
   #:image-alpha-get-code
   #:image-alpha-set-code
   #:image-alpha-blend-code
   ;; image ops
   #:def-rgb-image-primitives
   #:def-rgba-image-primitives
   #:def-gray-image-primitives
   #:def-stencil-image-primitives
   #:def-fast-copy-to-rgb-image
   ;; two dimensional array image
   #:rgb-image-pixels
   #:rgba-image-pixels
   #:single-channel-image-pixels
   ;; opticl image
   #:opticl-rgb-image-pixels
   #:opticl-rgba-image-pixels
   #:opticl-single-channel-image-pixels
   ))

(defpackage :mcclim-raster-internals
  (:use #:clim #:clim-lisp #:mcclim-raster #:mcclim-raster-extensions)
  (:import-from :clim-internals
                #:standard-color
                #:def-grecording
                #:defmethod*
                #:output-record-position
                #:defrecord-predicate
                #:with-standard-rectangle*
                #:coordinate=
                #:if-supplied)
  (:export))
