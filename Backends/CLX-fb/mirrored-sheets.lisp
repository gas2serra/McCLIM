(in-package :clim-clx-fb)

(defclass clx-fb-mirrored-sheet-mixin (fb-mirrored-sheet-mixin)
  ())

(defmethod sheet-direct-xmirror ((sheet clx-fb-mirrored-sheet-mixin))
  (when (sheet-direct-mirror sheet)
    (sheet-direct-xmirror (sheet-direct-mirror sheet))))

(defmethod sheet-direct-xmirror ((mirror clx-fb-mirror))
  (fb-mirror-real-mirror mirror))

(defmethod sheet-direct-xmirror ((mirror mcclim-render::image-mirror-mixin))
    nil)

(defclass clx-fb-pixmap (fb-pixmap)
  ())

(defmethod sheet-direct-xmirror ((sheet clx-fb-pixmap))
  nil)
