(in-package :mcclim-raster-internals)

;;;
;;; def image's primirives and functions
;;;
(defmacro def-rgb-image-primitives (image-class pixels-type pixels-var x-var y-var
                                   red-var green-var blue-var get-code set-code)
  `(progn
     (defmethod image-pixels-type ((image-class (eql ',image-class)))
       ',pixels-type)
     (defmethod image-rgb-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       ,get-code)
     (defmethod image-rgb-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                    ,red-var ,green-var ,blue-var)
       ,set-code)
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

(defmacro def-gray-image-primitives (image-class pixels-type pixels-var x-var y-var
                                    gray-var get-code set-code)
  `(progn
     (defmethod image-pixels-type ((image-class (eql ',image-class)))
       ',pixels-type)
     (defmethod image-gray-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       ,get-code)
     (defmethod image-gray-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var
                                     ,gray-var)
       ,set-code)
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

(defmacro def-image-functions (image-class)
  `(progn
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

(defmacro def-rgb-image-functions (image-class)
  `(progn
     (defmethod image-rgb-set-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y red green blue)
           (declare (type fixnum x y red green blue))
           ,(image-rgb-set-code image-class 'pixels '(+ x dx) '(+ y dy) 'red 'green 'blue))))
     (def-image-functions ,image-class)))

(defmacro def-rgba-image-functions (image-class)
  `(progn
     (defmethod image-rgba-set-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y red green blue alpha)
           (declare (type fixnum x y red green blue alpha))
           ,(image-rgba-set-code image-class 'pixels '(+ x dx) '(+ y dy) 'red 'green 'blue 'alpha))))
     (def-image-functions ,image-class)))

(defmacro def-gray-image-functions (image-class)
  `(progn
     (defmethod image-gray-set-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (lambda (x y gray)
           (declare (type fixnum x y gray))
           ,(image-gray-set-code image-class 'pixels '(+ x dx) '(+ y dy) 'gray))))
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

;;;
;;; copy functions
;;;

(defmacro do-copy-image (src-img sx sy width height dst-img x y (i-var j-var) &body code)
  `(progn
     (let ((max-y (+ ,y ,height -1))
           (max-x (+ ,x ,width -1)))
       (declare (type fixnum max-x max-y))
       (flet ((copy-ff ()
                (loop for ,j-var from y to max-y do
                     (loop for ,i-var from x to max-x do
                          ,@code)))
              (copy-bf ()
                  (loop for ,j-var from y to max-y do
                       (loop for ,i-var from max-x downto x do
                            ,@code)))
              (copy-fb ()
                  (loop for ,j-var from max-y downto y do
                       (loop for ,i-var from x to max-x do
                            ,@code)))
                (copy-bb ()
                  (loop for ,j-var from max-y downto y do
                       (loop for ,i-var from max-x downto x do
                            ,@code))))
         (when (and (> ,width 0) (> ,height 0))
           (if (eq ,src-img ,dst-img)
               (cond
                 ((and (<= ,sx ,x) (<= ,sy ,y))
                  (copy-bb))
                 ((and (<= ,sx ,x) (> ,sy ,y))
                  (copy-bf))
                 ((and (> ,sx ,x) (<= ,sy ,y))
                  (copy-fb))
                 ((and (> ,sx ,x) (> ,sy ,y))
                  (copy-ff)))
               (copy-ff))))
       (make-rectangle* ,x ,y (+ ,x ,width) (+ ,y ,height)))))

(defmethod copy-image :around ((src-img image-mixin) sx sy width height (dst-img image-mixin) x y)
  (call-next-method src-img (round sx) (round sy) (round width) (round height)
                    dst-img (round x) (round y)))

(defmethod copy-image ((src-img image-mixin) sx sy width height (dst-img rgb-image-mixin) x y)
  (declare (type fixnum sx sy width height x y))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (let ((src-get-fn (image-rgb-get-fn src-img :dx dx :dy dy))
          (dst-set-fn (image-rgb-set-fn dst-img)))
      (declare (type image-rgb-get-fn src-get-fn)
               (type image-rgb-set-fn dst-set-fn))
      (do-copy-image src-img sx sy width height dst-img x y (i j)
        (multiple-value-bind (red green blue)
            (funcall src-get-fn i j)
          (funcall dst-set-fn i j red green blue))))))

(defmethod copy-image ((src-img image-mixin) sx sy width height (dst-img rgba-image-mixin) x y)
  (declare (type fixnum sx sy width height x y))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (let ((src-get-fn (image-rgba-get-fn src-img :dx dx :dy dy))
          (dst-set-fn (image-rgba-set-fn dst-img)))
      (declare (type image-rgba-get-fn src-get-fn)
               (type image-rgba-set-fn dst-set-fn))
      (do-copy-image src-img sx sy width height dst-img x y (i j)
        (multiple-value-bind (red green blue alpha)
            (funcall src-get-fn i j)
          (funcall dst-set-fn i j red green blue alpha))))))

(defmethod copy-image ((src-img image-mixin) sx sy width height (dst-img gray-image-mixin) x y)
  (declare (type fixnum sx sy width height x y))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (let ((src-get-fn (image-gray-get-fn src-img :dx dx :dy dy))
          (dst-set-fn (image-gray-set-fn dst-img)))
      (declare (type image-gray-get-fn src-get-fn)
               (type image-gray-set-fn dst-set-fn))
      (do-copy-image src-img sx sy width height dst-img x y (i j)
        (multiple-value-bind (red green blue)
            (funcall dst-set-fn i j (funcall src-get-fn i j)))))))


(defmethod copy-image ((src-img image-mixin) sx sy width height (dst-img stencil-image-mixin) x y)
  (declare (type fixnum sx sy width height x y))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (let ((src-get-fn (image-alpha-get-fn src-img :dx dx :dy dy))
          (dst-set-fn (image-alpha-set-fn dst-img)))
      (declare (type image-alpha-get-fn src-get-fn)
               (type image-alpha-set-fn dst-set-fn))
      (do-copy-image src-img sx sy width height dst-img x y (i j)
        (funcall dst-set-fn i j (funcall src-get-fn i j))))))

(defmethod copy-image ((src-img stencil-image-mixin) sx sy width height
                       (dst-img gray-image-mixin) x y)
  (declare (type fixnum sx sy width height x y))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (let ((src-get-fn (image-alpha-get-fn src-img :dx dx :dy dy))
          (dst-set-fn (image-gray-set-fn dst-img)))
      (declare (type image-alpha-get-fn src-get-fn)
               (type image-gray-set-fn dst-set-fn))
      (do-copy-image src-img sx sy width height dst-img x y (i j)
        (funcall dst-set-fn i j (funcall src-get-fn i j))))))

(defmethod copy-image ((src-img gray-image-mixin) sx sy width height
                       (dst-img stencil-image-mixin) x y)
  (declare (type fixnum sx sy width height x y))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (let ((src-get-fn (image-gray-get-fn src-img :dx dx :dy dy))
          (dst-set-fn (image-alpha-set-fn dst-img)))
      (declare (type image-gray-get-fn src-get-fn)
               (type image-alpha-set-fn dst-set-fn))
      (do-copy-image src-img sx sy width height dst-img x y (i j)
        (funcall dst-set-fn i j (funcall src-get-fn i j))))))

;;;
;;; fast copy
;;;

(defmacro def-fast-copy-to-rgb-image (src-image-class dst-image-class)
  `(defmethod copy-image ((src-img ,src-image-class) sx sy width height (dst-img ,dst-image-class) x y)
     (declare (type fixnum sx sy width height x y))
     (let ((src-pixels (image-pixels src-img))
           (dst-pixels (image-pixels dst-img)))
       (declare (type ,(image-pixels-type src-image-class) src-pixels)
                (type ,(image-pixels-type dst-image-class) dst-pixels))
       (let ((dy (- sy y))
             (dx (- sx x)))
         (declare (type fixnum dx dy))
         (do-copy-image src-img sx sy width height dst-img x y (i j)
           (multiple-value-bind (red green blue)
               ,(image-rgb-get-code src-image-class 'src-pixels `(+ i dx) `(+ j dy))
             (declare (ignorable red green blue))
             ,(image-rgb-set-code dst-image-class 'dst-pixels 'i 'j 'red 'green 'blue)))))))

;;;
;;; coerce
;;;

(defmethod coerce-image ((image basic-image) image-class)
  (if (typep image image-class)
      image
      (clone-image image image-class)))

(defmethod clone-image ((image basic-image) image-class)
  (let ((dest (make-instance image-class
                             :width (image-width image)
                             :height (image-height image))))
    (copy-image image 0 0 (image-width image) (image-height image)
                dest 0 0)
    dest))
