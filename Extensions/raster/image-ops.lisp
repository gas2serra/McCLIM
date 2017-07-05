(in-package :mcclim-raster)

(defmacro mk-rgb-image-primitives (image-class pixels-type pixels-var x-var y-var red-var green-var blue-var get-code set-code)
  `(progn
     (defmethod image-pixels-type ((image-class (eql ',image-class)))
       ',pixels-type)
     (defmethod make-get-rgb-octets-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       ,get-code)
     (defmethod make-set-rgb-octets-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var ,red-var ,green-var ,blue-var)
       ,set-code)
     (defmethod make-get-rgba-octets-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       (multiple-value-bind (rc gc bc)
           ,get-code
         (values rc gc bc 255)))
     (defmethod make-set-rgba-octets-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var ,red-var ,green-var ,blue-var alpha-var)
       (declare (ignore alpha-var))
       ,set-code)))

(defmacro mk-rgba-image-primitives (image-class pixels-type pixels-var x-var y-var red-var green-var blue-var alpha-var get-code set-code)
  `(progn
     (defmethod image-pixels-type ((image-class (eql ',image-class)))
       ',pixels-type)
     (defmethod make-get-rgb-octets-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
       ,get-code)
     (defmethod make-set-rgb-octets-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var ,red-var ,green-var ,blue-var)
       (let ((,alpha-var 255))
         ,set-code))
     (defmethod make-get-rgba-octets-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var)
           ,get-code)
     (defmethod make-set-rgba-octets-code ((image-class (eql ',image-class)) ,pixels-var ,x-var ,y-var ,red-var ,green-var ,blue-var alpha-var)
       ,set-code)))

;;;
;;; map functions
;;;
(defmacro make-map-rgb-color (image-class)
  `(defmethod map-rgb-color ((image ,image-class) fn)
     (let ((pixels (image-pixels image)))
       (declare (type ,(image-pixels-type image-class) pixels))
       (dotimes (x (1- (image-width image)))
         (dotimes (y (1- (image-height image)))
           (multiple-value-bind (red green blue)
               ,(make-get-rgb-octets-code image-class 'pixels 'x 'y)
             (funcall fn x y red green blue)))))))
;;;
;;; copy functions
;;;

(defmacro make-copy-image (src-image-class dst-image-class)
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
                                ,(make-get-rgba-octets-code src-image-class 'src-pixels `(+ i dx) `(+ j dy))
                              ,(make-set-rgba-octets-code dst-image-class 'dst-pixels 'i 'j 'red 'green 'blue 'alpha)))))
                (copy-bf ()
                  (loop for j from y to max-y do
                       (loop for i from max-x downto x do
                            (multiple-value-bind (red green blue alpha)
                                ,(make-get-rgba-octets-code src-image-class 'src-pixels `(+ i dx) `(+ j dy))
                              ,(make-set-rgba-octets-code dst-image-class 'dst-pixels 'i 'j 'red 'green 'blue 'alpha)))))
                (copy-fb ()
                  (loop for j from max-y downto y do
                       (loop for i from x to max-x do
                            (multiple-value-bind (red green blue alpha)
                                ,(make-get-rgba-octets-code src-image-class 'src-pixels `(+ i dx) `(+ j dy))
                              ,(make-set-rgba-octets-code dst-image-class 'dst-pixels 'i 'j 'red 'green 'blue 'alpha)))))
                (copy-bb ()
                  (loop for j from max-y downto y do
                       (loop for i from max-x downto x do
                            (multiple-value-bind (red green blue alpha)
                                ,(make-get-rgba-octets-code src-image-class 'src-pixels `(+ i dx) `(+ j dy))
                              ,(make-set-rgba-octets-code dst-image-class 'dst-pixels 'i 'j 'red 'green 'blue 'alpha))))))
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
