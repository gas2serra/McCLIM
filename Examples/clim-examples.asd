
;;; CLIM-Examples depends on having at least one backend loaded.
(asdf:defsystem #:clim-examples
    :depends-on (#:mcclim #:mcclim-layouts/tab :mcclim-raster-image)
    :depends-on (#:mcclim #:mcclim-layouts/tab :mcclim-raster-image #:mcclim-bezier)
    :components
    ((:file "package")
     (:file "calculator")
     (:file "colorslider")
     (:file "menutest")                 ; extra
     (:file "address-book")
     (:file "traffic-lights")
     (:file "clim-fig")
     (:file "postscript-test")
     (:file "puzzle")
     (:file "transformations-test")
     (:file "town-example")
     (:file "demodemo" :depends-on ("tabdemo" "town-example" "sliderdemo"))
     (:file "stream-test")
     (:file "presentation-test")
     (:file "dragndrop")
     (:file "gadget-test")
     (:file "method-browser")
     (:file "stopwatch")
     (:file "dragndrop-translator")
     (:file "draggable-graph")
     (:file "text-size-test")
     (:file "drawing-benchmark")
     (:file "logic-cube")
     (:file "views")
     (:file "font-selector")
     (:file "tabdemo")
     (:file "bordered-output-examples")
     (:file "misc-tests")
     (:file "drawing-tests")
     (:file "image-viewer")
     (:file "sliderdemo")
     (:file "accepting-values")
     (:file "accepting-values-test")))

(asdf:defsystem #:clim-examples/superapp
  :depends-on (#:mcclim #:bordeaux-threads)
  :components ((:file "superapp")))
