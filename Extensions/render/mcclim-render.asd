
(defsystem #:mcclim-render/image
    :depends-on (#:clim-basic #:mcclim-fonts/truetype #:opticl #:mcclim-image #:mcclim-raster)
    :serial t
    :components
    ((:file "package")
     (:file "image")
     (:file "vectors")
     (:file "vectors-image-ops")
     (:file "2d-image")
     (:file "opticl-image")
     ))

(defsystem #:mcclim-render
    :depends-on (#:mcclim-render/image)
    :serial t
    :components
    ((:file "prim-arc")
     (:file "prim-text")))

(defsystem #:mcclim-render/backend
    :depends-on (#:mcclim-render)
    :serial t
    :components
    ((:file "mirror")
     (:file "opticl-mirror")
     (:file "mirrored-sheet")
     (:file "pixmap")
     (:file "medium")
     (:file "fonts")
     (:file "port")))

(defsystem #:mcclim-render/clx
    :depends-on (#:mcclim-render/backend)
    :serial t
    :components
    ())
