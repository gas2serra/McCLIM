(in-package :clim-sdl2)


;;;
;;; frame manager
;;;

(defclass sdl2-fb-frame-manager (fb-frame-manager)
  ())

(defmethod fb-frame-manager-mirrored-sheet-mixin-class ((fm sdl2-fb-frame-manager))
  'sdl2-fb-mirrored-sheet-mixin)
