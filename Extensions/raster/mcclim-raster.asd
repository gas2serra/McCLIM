(defsystem #:mcclim-raster
    :description "Support for raster images McCLIM."
    :depends-on (#:mcclim-raster/core
                 #:mcclim-raster/two-dim-array
                 #:mcclim-raster/opticl))

(defsystem #:mcclim-raster/core
  :depends-on (#:clim-basic)
  :serial t
  :components ((:file "package")
               (:file "color")
               (:file "image")
               (:file "image-ops")))

(defsystem #:mcclim-raster/two-dim-array
  :depends-on (#:mcclim-raster/core)
  :serial t
  :components ((:file "two-dim-array-image")))

(defsystem #:mcclim-raster/opticl
  :depends-on (#:mcclim-raster/two-dim-array #:opticl)
  :serial t
  :components ((:file "opticl-image")))

(defsystem #:mcclim-raster/clx
  :depends-on (#:mcclim-clx/output #:mcclim-raster/two-dim-array)
  :serial t
  :components ((:file "clx-extension")))
