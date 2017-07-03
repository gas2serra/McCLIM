(in-package :clim-fb)

(defclass fb-mirrored-sheet-mixin (image-sheet-mixin
                                   standard-single-mirrored-sheet-mixin)
  ())

(defmethod allocate-space :after ((pane fb-mirrored-sheet-mixin) width height)
  (with-slots (space-requirement) pane
    (when (sheet-mirror pane)
      (fb-mirror-set-wm-size-hints (sheet-mirror pane) width height space-requirement))))

(defclass fb-pixmap (image-pixmap-mixin permanent-medium-sheet-output-mixin basic-pane)
  ())
