
(defsystem #:clim
  :depends-on (#:clim-core #:clim-standard #:drei-mcclim)
  :components ((:file "input-editing-drei")
               (:file "text-editor-gadget")))
