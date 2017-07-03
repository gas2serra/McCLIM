(in-package :clim-clx-fb)


(defclass clx-fb-frame-manager (fb-frame-manager)
  ())

(defmethod fb-frame-manager-mirrored-sheet-mixin-class ((fm clx-fb-frame-manager))
  'clx-fb-mirrored-sheet-mixin)

