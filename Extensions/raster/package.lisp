(defpackage :mcclim-raster
  (:nicknames #:clim-image)
  (:use #:clim #:clim-lisp)
  (:export
   ;; colors
   #:octet
   #:color-value->octet
   #:color-octet->value
   #:color-octet-xor
   #:octet-mult
   #:octet-blend-function
   ;; image
   #:image
   #:image-width
   #:image-height
   #:rgb-image-mixin
   #:drawable-image
   #:map-rgb-color
   #:draw-image*
   #:medium-draw-image*
   #:basic-image
   #:image-pixels
   #:image-pixels-type
   #:make-get-rgb-octets-code
   #:make-set-rgb-octets-code
   #:make-get-rgba-octets-code
   #:make-set-rgba-octets-code
   #:read-image
   #:write-image
   #:image-format-supported-p
   ;; image ops
   #:make-map-rgb-color
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
