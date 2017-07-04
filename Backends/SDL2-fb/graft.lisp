(in-package :clim-sdl2)

(defclass sdl2-graft (graft)
  ())

(defmethod graft-width ((graft sdl2-graft) &key (units :device))
  (declare (ignore units))
  ;;int SDL_GetCurrentDisplayMode(int              displayIndex,
  ;;                            SDL_DisplayMode* mode)
  1000)

(defmethod graft-height ((graft sdl2-graft) &key (units :device))
  (declare (ignore units))
  1000)



