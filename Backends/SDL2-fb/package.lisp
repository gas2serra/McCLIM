;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package :common-lisp-user)

(defpackage :clim-sdl2
  (:use :clim :clim-lisp :clim-fb :clim-backend)
  (:import-from :climi
		#:frame-managers
		#:port-grafts
		#:port-event-process
		#:port-lookup-mirror
		#:port-lookup-sheet
		#:port-register-mirror
		#:port-unregister-mirror
		#:top-level-sheet-pane
		#:window-destroy-event
		#:port-set-mirror-region
                #:port-pointer
                #:standard-pointer
                #:synthesize-pointer-motion-event
		)
    (:import-from :mcclim-render
                  #:image-width
                  #:image-height
		  )
    (:import-from :clim-standard
		#:standard-port
		#:standard-single-mirrored-sheet-mixin
		#:standard-handled-event-port-mixin
		#:%sheet-mirror-transformation
		#:%sheet-mirror-region
		))

