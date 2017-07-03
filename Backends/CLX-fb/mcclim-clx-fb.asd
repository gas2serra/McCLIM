
(defsystem #:mcclim-clx-fb
    :depends-on (#:mcclim-clx/basic
		 #:mcclim-clx/input
		 #:mcclim-fb
                 #:mcclim-render/clx)
    :components
    ((:file "package")
     (:file "port" :depends-on ("package"))
     (:file "frame-manager" :depends-on ("port" "package" "mirrored-sheets"))
     (:file "mirror" :depends-on ("port" "package"))
     (:file "mirrored-sheets" :depends-on ("port" "package" "mirror"))))
