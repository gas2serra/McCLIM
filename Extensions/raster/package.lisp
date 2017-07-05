(defpackage :mcclim-raster
  (:nicknames #:clim-image)
  (:use #:clim #:clim-lisp)
  (:import-from :climi
                #:standard-color
                #:def-grecording
                #:defmethod*
                #:output-record-position
                #:defrecord-predicate
                #:with-standard-rectangle*
                #:coordinate=
                #:if-supplied)
  (:export
   ;; colors
   #:octet
   #:color-value->octet
   #:color-octet->value
   #:color-octet-xor
   #:octet-mult
   #:octet-blend-function
   #:color->octets
   ;; image
   #:image
   #:image-width
   #:image-height
   #:rgb-image-mixin
   #:drawable-image
   #:map-rgb-color
   #:draw-image*
   #:medium-draw-image*
   #:image-design
   #:make-image-design
   #:basic-image
   #:image-pixels
   #:image-pixels-type
   #:image-rgb-get-code
   #:image-rgb-set-code
   #:image-rgba-get-code
   #:image-rgba-set-code
   #:image-rgb-get-fn
   #:image-rgb-set-fn
   #:image-rgba-get-fn
   #:image-rgba-set-fn
   ;; image ops
   #:mk-rgb-image-primitives
   #:mk-rgba-image-primitives
   #:mk-fast-copy-image
   #:read-image
   #:write-image
   #:image-format-read-supported-p
   #:image-format-write-supported-p
   #:coerce-image
   #:clone-image
   #:copy-image
   ;; two dimensional array image
   #:two-dim-array-image
   #:rgb-image-pixels
   #:rgb-image
   #:make-rgb-image
   ;; opticl image
   #:opticl-image
   #:opticl-rgb-image-pixels
   #:opticl-rgb-image
   #:make-opticl-rgb-image
   #:opticl-rgba-image-pixels
   #:opticl-rgba-image
   #:make-opticl-rgba-image
   ))
