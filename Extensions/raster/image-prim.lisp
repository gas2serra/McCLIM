(in-package :mcclim-raster-internals)

;;;
;;; def image's primirives
;;;
(defmacro def-rgba-image-primitives (image-class pixels-type pixels-var x-var y-var
                                    red-var green-var blue-var alpha-var get-code set-code)
  `(progn
     (defmethod image-pixels-type ((image-class (eql ',image-class)))
       ',pixels-type)
     (defmethod image-rgba-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       ,get-code)
     (defmethod image-rgba-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                     ,red-var ,green-var ,blue-var alpha-var)
       ,set-code)
      (defmethod image-rgba-blend-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                        ,red-var ,green-var ,blue-var ,alpha-var)
        `(multiple-value-bind (r g b a)
             ,,get-code
           (multiple-value-bind (,,red-var ,,green-var ,,blue-var ,,alpha-var)
               (octet-rgba-blend-function ,,red-var ,,green-var ,,blue-var ,,alpha-var
                                          r g b a)
             ,,set-code)))
     (defmethod image-rgb-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       ,get-code)
     (defmethod image-gray-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       `(multiple-value-bind (rc gc bc)
            ,,get-code
          (floor (+ rc gc bc) 3)))
     (defmethod image-alpha-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       `(multiple-value-bind (rc gc bc a)
            ,,get-code
          (declare (ignore rc gc bc))
          a))))

(defmacro def-rgb-image-primitives (image-class pixels-type pixels-var x-var y-var
                                   red-var green-var blue-var alpha-var get-code set-code)
  `(progn
     (defmethod image-pixels-type ((image-class (eql ',image-class)))
       ',pixels-type)
     (defmethod image-rgb-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       ,get-code)
     (defmethod image-rgb-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                    ,red-var ,green-var ,blue-var)
       ,set-code)
     (defmethod image-rgb-blend-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                      ,red-var ,green-var ,blue-var ,alpha-var)
       `(multiple-value-bind (r g b)
            ,,get-code
          (multiple-value-bind (,,red-var ,,green-var ,,blue-var)
              (octet-rgb-blend-function ,,red-var ,,green-var ,,blue-var ,,alpha-var
                                        r g b)
            ,,set-code)))
     (defmethod image-rgba-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       `(multiple-value-bind (rc gc bc)
            ,,get-code
          (values rc gc bc 255)))
     (defmethod image-gray-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       `(multiple-value-bind (rc gc bc)
            ,,get-code
          (floor (+ rc gc bc) 3)))
     (defmethod image-alpha-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       0)
     ;; to remove... compatibility
     (defmethod image-rgba-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                     ,red-var ,green-var ,blue-var alpha-var)
       (declare (ignore alpha-var))
       ,set-code)))

(defmacro def-gray-image-primitives (image-class pixels-type pixels-var x-var y-var
                                    gray-var alpha-var get-code set-code)
  `(progn
     (defmethod image-pixels-type ((image-class (eql ',image-class)))
       ',pixels-type)
     (defmethod image-gray-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       ,get-code)
     (defmethod image-gray-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                     ,gray-var)
       ,set-code)
     (defmethod image-gray-blend-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                       ,gray-var ,alpha-var)
       `(multiple-value-bind (g) ;; use gensymb
            ,,get-code
          (multiple-value-bind (,,gray-var)
              (octet-gray-blend-function ,,gray-var ,,alpha-var
                                         g)
            ,,set-code)))
     (defmethod image-rgb-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       `(let ((gray ,,get-code))
          (values gray gray gray)))
     (defmethod image-rgba-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       `(let ((gray ,,get-code))
          (values gray gray gray 255)))
     (defmethod image-alpha-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       0)))

(defmacro def-stencil-image-primitives (image-class pixels-type pixels-var
                                       x-var y-var alpha-var get-code set-code)
  `(progn
     (defmethod image-pixels-type ((image-class (eql ',image-class)))
       ',pixels-type)
     (defmethod image-alpha-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                      ,alpha-var)
       ,set-code)
     (defmethod image-alpha-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       ,get-code)
     (defmethod image-gray-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       0)
     (defmethod image-rgb-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       (values 0 0 0))
     (defmethod image-rgba-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       `(let ((alpha ,,get-code))
          (values 0 0 0 alpha)))))

;;;
;;; def image's functions
;;;
(defmacro def-image-functions (image-class)
  `(progn
     (defmethod image-rgba-get-fn ((image ,image-class) &key (dx 0) (dy 0) (region nil))
       (declare (ignorable dx dy))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type fixnum dx dy)
                  (ignorable pixels))
         (lambda (x y)
           (declare (type fixnum x y))
           (if (or (not region) (clim:region-contains-position-p region x y))
               ,(image-rgb-get-code image-class 'pixels '(+ x dx) '(+ y dy))
               (values 0 0 0)))))
     (defmethod image-rgb-get-fn ((image ,image-class) &key (dx 0) (dy 0) (region nil))
       (declare (ignorable dx dy))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type fixnum dx dy)
                  (ignorable pixels))
         (lambda (x y)
           (declare (type fixnum x y))
           (if (or (not region) (clim:region-contains-position-p region x y))
               ,(image-rgb-get-code image-class 'pixels '(+ x dx) '(+ y dy))
               (values 0 0 0)))))
     (defmethod image-gray-get-fn ((image ,image-class) &key (dx 0) (dy 0) (region nil))
       (declare (ignorable dx dy))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type fixnum dx dy)
                  (ignorable pixels))
         (lambda (x y)
           (declare (type fixnum x y))
           (if (or (not region) (clim:region-contains-position-p region x y))
               ,(image-gray-get-code image-class 'pixels '(+ x dx) '(+ y dy))
               0))))
     (defmethod image-gray-alpha-get-fn ((image ,image-class) &key (dx 0) (dy 0) (region nil))
       (declare (ignorable dx dy))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type fixnum dx dy)
                  (ignorable pixels))
         (lambda (x y)
           (declare (type fixnum x y))
           (if (or (not region) (clim:region-contains-position-p region x y))
               (multiple-value-bind (r g b a)
                   ,(image-rgba-get-code image-class 'pixels '(+ x dx) '(+ y dy))
                 (values (round (+ r g b) 3) a))
               0))))
     (defmethod image-alpha-get-fn ((image ,image-class) &key (dx 0) (dy 0) (region nil))
       (declare (ignorable dx dy))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type fixnum dx dy)
                  (ignorable pixels))
         (lambda (x y)
           (declare (type fixnum x y))
           (if (or (not region) (clim:region-contains-position-p region x y))
               ,(image-alpha-get-code image-class 'pixels '(+ x dx) '(+ y dy))
               0))))
     (defmethod map-rgb-color ((image ,image-class) fn &key (x 0) (y 0)
                                                         (width (image-width image))
                                                         (height (image-height image)))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type fixnum x y width height)
                  (ignorable pixels))
         (loop for i from x to (+ x width -1) do
              (loop for j from y to (+ y height -1) do
                   (multiple-value-bind (red green blue)
                       ,(image-rgb-get-code image-class 'pixels 'i 'j)
                     (funcall fn i j red green blue))))))))

(defmacro def-rgba-image-functions (image-class)
  `(progn
     (defmethod image-rgba-set-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y red green blue alpha)
           (declare (type fixnum x y red green blue alpha))
           ,(image-rgba-set-code image-class 'pixels '(+ x dx) '(+ y dy) 'red 'green 'blue 'alpha))))
     (defmethod image-rgba-blend-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y red green blue alpha)
           (declare (type fixnum x y red green blue alpha))
           ,(image-rgba-blend-code image-class 'pixels '(+ x dx) '(+ y dy) 'red 'green 'blue 'alpha))))
     (def-image-functions ,image-class)))

(defmacro def-rgb-image-functions (image-class)
  `(progn
     (defmethod image-rgb-set-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y red green blue)
           (declare (type fixnum x y red green blue))
           ,(image-rgb-set-code image-class 'pixels '(+ x dx) '(+ y dy) 'red 'green 'blue))))
     (defmethod image-rgb-blend-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y red green blue alpha)
           (declare (type fixnum x y red green blue alpha))
           ,(image-rgb-blend-code image-class 'pixels '(+ x dx) '(+ y dy) 'red 'green 'blue 'alpha))))
     (def-image-functions ,image-class)))

(defmacro def-gray-image-functions (image-class)
  `(progn
     (defmethod image-gray-set-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y gray)
           (declare (type fixnum x y gray))
           ,(image-gray-set-code image-class 'pixels '(+ x dx) '(+ y dy) 'gray))))
      (defmethod image-gray-blend-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y gray alpha)
           (declare (type fixnum x y gray alpha))
           ,(image-gray-blend-code image-class 'pixels '(+ x dx) '(+ y dy) 'gray 'alpha))))
     (def-image-functions ,image-class)))

(defmacro def-stencil-image-functions (image-class)
  `(progn
     (defmethod image-alpha-set-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y alpha)
           (declare (type fixnum x y alpha))
           ,(image-alpha-set-code image-class 'pixels '(+ x dx) '(+ y dy) 'alpha))))
     (def-image-functions ,image-class)))
