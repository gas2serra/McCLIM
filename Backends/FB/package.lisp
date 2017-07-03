;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package :common-lisp-user)

(defpackage :clim-fb
  (:use :clim :clim-lisp :clim-backend)
  (:import-from :climi
                #:permanent-medium-sheet-output-mixin
                #:basic-medium
                #:realize-mirror
                #:port-register-mirror
                #:port-lookup-mirror
                #:mirrored-sheet-mixin
                #:top-level-sheet-pane)
  (:import-from :mcclim-render
                #:image-sheet-mixin
                #:render-port-mixin
                #:render-medium-mixin
                #:image-pixmap-mixin
                #:opticl-rgb-image-mirror-mixin
                #:%make-image
                #:%mirror-force-output
                #:%create-mirror-image)
  (:import-from :clim-standard
                #:standard-port
                #:standard-single-mirrored-sheet-mixin
                #:standard-handled-event-port-mixin)
  (:export
   #:fb-medium
   ;; port
   #:fb-port
   #:fb-port-flush-mirrors
   #:fb-port-realize-real-mirror
   #:fb-port-realize-mirror
   #:fb-port-realize-pixmap
   ;; mirror
   #:fb-mirror
   #:fb-mirror-buffer
   #:fb-mirror-real-mirror
   #:fb-mirror-dirty-region-set
   #:fb-mirror-create-buffer
   #:fb-mirror-destroy-buffer
   #:fb-mirror-copy-to-buffer
   #:fb-mirror-flush-buffer
   #:fb-mirror-set-wm-size-hints
   

   #:fb-mirrored-sheet-mixin
   #:fb-pixmap
   
   ;; frame manager
   #:fb-frame-manager
   #:fb-frame-manager-mirrored-sheet-mixin-class   
   ))
