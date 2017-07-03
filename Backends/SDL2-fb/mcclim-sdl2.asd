
(defsystem #:mcclim-sdl2
  :depends-on (#:clim #:mcclim-fb  #:sdl2 :lparallel)
  :components
  ((:file "package")
   (:file "server" :depends-on ("package"))
   (:file "port" :depends-on ("package" "mirror"))
   (:file "mirror" :depends-on ("package"))
   (:file "medium" :depends-on ("port" "package"))
   (:file "graft" :depends-on ("port" "package"))
   (:file "frame-manager" :depends-on ("medium" "port" "package"))
   (:file "mirrored-sheets" :depends-on ("port" "package"))))


