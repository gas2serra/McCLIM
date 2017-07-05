(in-package :mcclim-raster)

(defmacro mk-rgb-image-primitives (image-class pixels-type pixels-var x-var y-var red-var green-var blue-var get-code set-code)
  `(progn
     (defmethod image-pixels-type ((image-class (eql ',image-class)))
       ',pixels-type)
     (defmethod image-rgb-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       ,get-code)
     (defmethod image-rgb-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var ,red-var ,green-var ,blue-var)
       ,set-code)
     (defmethod image-rgba-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       (multiple-value-bind (rc gc bc)
           ,get-code
         (values rc gc bc 255)))
     (defmethod image-rgba-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var ,red-var ,green-var ,blue-var alpha-var)
       (declare (ignore alpha-var))
       ,set-code)))

(defmacro mk-rgba-image-primitives (image-class pixels-type pixels-var x-var y-var red-var green-var blue-var alpha-var get-code set-code)
  `(progn
     (defmethod image-pixels-type ((image-class (eql ',image-class)))
       ',pixels-type)
     (defmethod image-rgb-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       ,get-code)
     (defmethod image-rgb-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var ,red-var ,green-var ,blue-var)
       (let ((,alpha-var 255))
         ,set-code))
     (defmethod image-rgba-get-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
           ,get-code)
     (defmethod image-rgba-set-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var ,red-var ,green-var ,blue-var alpha-var)
       ,set-code)))

(defmacro mk-image-functions (image-class)
  `(progn
     (defmethod image-rgb-get-fn ((image ,image-class) &key (dx 0) (dy 0) (region nil))
       (let ((data (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) data)
                  (type fixnum dx dy))
         (lambda (x y)
           (declare (type fixnum x y))
           (if (or (not region) (clim:region-contains-position-p region x y))
               ,(image-rgb-get-code image-class 'data '(+ x dx) '(+ y dy))
               (values 0 0 0)))))
     (defmethod image-rgba-get-fn ((image ,image-class) &key (dx 0) (dy 0) (region nil))
       (let ((data (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) data)
                  (type fixnum dx dy))
         (lambda (x y)
           (declare (type fixnum x y))
           (if (or (not region) (clim:region-contains-position-p region x y))
               ,(image-rgb-get-code image-class 'data '(+ x dx) '(+ y dy))
               (values 0 0 0)))))
     (defmethod map-rgb-color ((image ,image-class) fn &key (x 0) (y 0)
                                                         (width (image-width image))
                                                         (height (image-height image)))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type fixnum x y width height))
         (loop for i from x to (+ x width -1) do
              (loop for j from y to (+ y height -1) do
                   (multiple-value-bind (red green blue)
                       ,(image-rgb-get-code image-class 'pixels 'i 'j)
                     (funcall fn i j red green blue))))))))

(defmacro mk-rgb-image-functions (image-class)
  `(progn
     (defmethod image-rgb-set-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((data (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) data))
         (lambda (x y red green blue)
           (declare (type fixnum x y red green blue))
           ,(image-rgb-set-code image-class 'data '(+ x dx) '(+ y dy) 'red 'green 'blue))))))

(defmacro mk-rgba-image-functions (image-class)
  `(progn
     (defmethod image-rgba-set-fn ((image ,image-class) &key (dx 0) (dy 0))
       (let ((data (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) data))
         (lambda (x y red green blue alpha)
           (declare (type fixnum x y red green blue alpha))
           ,(image-rgba-set-code image-class 'data '(+ x dx) '(+ y dy) 'red 'green 'blue 'alpha))))))

;;;
;;; copy functions
;;;

(defmethod copy-image ((src-img image) sx sy width height (dst-img rgb-image-mixin) x y)
  (declare (type fixnum sx sy width height x y))
  (let ((max-y (+ y height -1))
        (max-x (+ x width -1))
        (dy (- sy y))
        (dx (- sx x)))
    (declare (type fixnum max-x max-y dx dy))
    (let ((src-get-fn (image-rgb-get-fn src-img :dx dx :dy dy))
          (dst-set-fn (image-rgb-set-fn dst-img)))
      (declare (type image-rgb-get-fn src-get-fn)
               (type image-rgb-set-fn dst-set-fn))
      (flet ((copy-ff ()
               (loop for j from y to max-y do
                    (loop for i from x to max-x do
                         (multiple-value-bind (red green blue)
                             (funcall src-get-fn i j)
                           (funcall dst-set-fn i j red green blue)))))
             (copy-bf ()
               (loop for j from y to max-y do
                    (loop for i from max-x downto x do
                         (multiple-value-bind (red green blue)
                             (funcall src-get-fn i j)
                           (funcall dst-set-fn i j red green blue)))))
             (copy-fb ()
               (loop for j from max-y downto y do
                    (loop for i from x to max-x do
                         (multiple-value-bind (red green blue)
                             (funcall src-get-fn i j)
                           (funcall dst-set-fn i j red green blue)))))
             (copy-bb ()
               (loop for j from max-y downto y do
                    (loop for i from max-x downto x do
                         (multiple-value-bind (red green blue)
                             (funcall src-get-fn i j)
                           (funcall dst-set-fn i j red green blue))))))
        (when (and (> width 0) (> height 0))
          (if (eq src-img dst-img)
              (cond
                ((and (<= sx x) (<= sy y))
                 (copy-bb))
                   ((and (<= sx x) (> sy y))
                    (copy-bf))
                   ((and (> sx x) (<= sy y))
                    (copy-fb))
                   ((and (> sx x) (> sy y))
                    (copy-ff)))
              (copy-ff)))))
    (make-rectangle* x y (+ x width) (+ y height))))

(defmethod copy-image ((src-img image) sx sy width height (dst-img rgba-image-mixin) x y)
  (declare (type fixnum sx sy width height x y))
  (let ((max-y (+ y height -1))
        (max-x (+ x width -1))
        (dy (- sy y))
        (dx (- sx x)))
    (declare (type fixnum max-x max-y dx dy))
    (let ((src-get-fn (image-rgb-get-fn src-img :dx dx :dy dy))
          (dst-set-fn (image-rgb-set-fn dst-img)))
      (declare (type image-rgba-get-fn src-get-fn)
               (type image-rgba-set-fn dst-set-fn))
      (flet ((copy-ff ()
               (loop for j from y to max-y do
                    (loop for i from x to max-x do
                         (multiple-value-bind (red green blue alpha)
                             (funcall src-get-fn i j)
                           (funcall dst-set-fn i j red green blue alpha)))))
             (copy-bf ()
               (loop for j from y to max-y do
                    (loop for i from max-x downto x do
                         (multiple-value-bind (red green blue alpha)
                             (funcall src-get-fn i j)
                           (funcall dst-set-fn i j red green blue alpha)))))
             (copy-fb ()
               (loop for j from max-y downto y do
                    (loop for i from x to max-x do
                         (multiple-value-bind (red green blue alpha)
                             (funcall src-get-fn i j)
                           (funcall dst-set-fn i j red green blue alpha)))))
             (copy-bb ()
               (loop for j from max-y downto y do
                    (loop for i from max-x downto x do
                         (multiple-value-bind (red green blue alpha)
                             (funcall src-get-fn i j)
                           (funcall dst-set-fn i j red green blue alpha))))))
        (when (and (> width 0) (> height 0))
          (if (eq src-img dst-img)
              (cond
                ((and (<= sx x) (<= sy y))
                 (copy-bb))
                ((and (<= sx x) (> sy y))
                 (copy-bf))
                ((and (> sx x) (<= sy y))
                 (copy-fb))
                ((and (> sx x) (> sy y))
                 (copy-ff)))
              (copy-ff)))))
    (make-rectangle* x y (+ x width) (+ y height))))

(defmacro mk-fast-copy-image (src-image-class dst-image-class)
  `(defmethod copy-image ((src-img ,src-image-class) sx sy width height (dst-img ,dst-image-class) x y)
     (declare (type fixnum sx sy width height x y))
     (let ((src-pixels (image-pixels src-img))
           (dst-pixels (image-pixels dst-img)))
       (declare (type ,(image-pixels-type src-image-class) src-pixels)
                (type ,(image-pixels-type dst-image-class) dst-pixels))
       (let ((max-y (+ y height -1))
             (max-x (+ x width -1))
             (dy (- sy y))
             (dx (- sx x)))
         (declare (type fixnum max-x max-y dx dy))
         (flet ((copy-ff ()
                  (loop for j from y to max-y do
                       (loop for i from x to max-x do
                            (multiple-value-bind (red green blue alpha)
                                ,(image-rgba-get-code src-image-class 'src-pixels `(+ i dx) `(+ j dy))
                              (declare (ignorable alpha))
                              ,(image-rgba-set-code dst-image-class 'dst-pixels 'i 'j 'red 'green 'blue 'alpha)))))
                (copy-bf ()
                  (loop for j from y to max-y do
                       (loop for i from max-x downto x do
                            (multiple-value-bind (red green blue alpha)
                                ,(image-rgba-get-code src-image-class 'src-pixels `(+ i dx) `(+ j dy))
                              (declare (ignorable alpha))
                              ,(image-rgba-set-code dst-image-class 'dst-pixels 'i 'j 'red 'green 'blue 'alpha)))))
                (copy-fb ()
                  (loop for j from max-y downto y do
                       (loop for i from x to max-x do
                            (multiple-value-bind (red green blue alpha)
                                ,(image-rgba-get-code src-image-class 'src-pixels `(+ i dx) `(+ j dy))
                              (declare (ignorable alpha))
                              ,(image-rgba-set-code dst-image-class 'dst-pixels 'i 'j 'red 'green 'blue 'alpha)))))
                (copy-bb ()
                  (loop for j from max-y downto y do
                       (loop for i from max-x downto x do
                            (multiple-value-bind (red green blue alpha)
                                ,(image-rgba-get-code src-image-class 'src-pixels `(+ i dx) `(+ j dy))
                              (declare (ignorable alpha))
                              ,(image-rgba-set-code dst-image-class 'dst-pixels 'i 'j 'red 'green 'blue 'alpha))))))
           (when (and (> width 0) (> height 0))
             (if (eq src-img dst-img)
                 (cond
                   ((and (<= sx x) (<= sy y))
                    (copy-bb))
                   ((and (<= sx x) (> sy y))
                    (copy-bf))
                   ((and (> sx x) (<= sy y))
                    (copy-fb))
                   ((and (> sx x) (> sy y))
                    (copy-ff)))
                 (copy-ff)))))
       (make-rectangle* x y (+ x width) (+ y height)))))

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
