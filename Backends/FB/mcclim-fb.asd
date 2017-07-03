(defsystem #:mcclim-fb
    :depends-on (#:mcclim-single-mirrored-standard
		 #:mcclim-render)
    :components
    ((:file "package")
     (:file "port" :depends-on ("package" "medium" "mirrored-sheets"))
     (:file "medium" :depends-on ("package"))
     (:file "frame-manager" :depends-on ("port" "package" "mirrored-sheets"))
     (:file "mirrored-sheets" :depends-on ("package" "mirror"))
     (:file "mirror" :depends-on ("package"))))
