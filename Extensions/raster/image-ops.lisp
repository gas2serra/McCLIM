(in-package :mcclim-raster-internals)

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

(defmethod copy-image :around ((src-img image-mixin) sx sy width height
                               (dst-img image-mixin) x y)
  ;; TO FIX: check image bounds
  (call-next-method src-img (round sx) (round sy) (round width) (round height)
                    dst-img (round x) (round y)))

(defmethod copy-image ((src-img image-mixin) sx sy width height
                       (dst-img rgba-image-mixin) x y)
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

(defmethod copy-image ((src-img image-mixin) sx sy width height
                       (dst-img rgb-image-mixin) x y)
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

(defmethod copy-image ((src-img image-mixin) sx sy width height
                       (dst-img gray-image-mixin) x y)
  (declare (type fixnum sx sy width height x y))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (let ((src-get-fn (image-gray-get-fn src-img :dx dx :dy dy))
          (dst-set-fn (image-gray-set-fn dst-img)))
      (declare (type image-gray-get-fn src-get-fn)
               (type image-gray-set-fn dst-set-fn))
      (do-copy-image src-img sx sy width height dst-img x y (i j)
        (funcall dst-set-fn i j (funcall src-get-fn i j))))))

(defmethod copy-image ((src-img alpha-channel-image-mixin) sx sy width height
                       (dst-img alpha-channel-image-mixin) x y)
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

;; fast copy
(defmacro def-fast-copy-to-rgb-image (src-image-class dst-image-class)
  `(defmethod copy-image ((src-img ,src-image-class) sx sy width height
                          (dst-img ,dst-image-class) x y)
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
;;; blend functions
;;;
(defmethod blend-image :around ((src-img image-mixin) sx sy width height
                                (dst-img image-mixin) x y &key (alpha 255))
  ;; TO FIX: check image bounds
  (call-next-method src-img (round sx) (round sy) (round width) (round height)
                    dst-img (round x) (round y) :alpha (round alpha)))

(defmethod blend-image ((src-img image-mixin) sx sy width height
                        (dst-img rgba-image-mixin) x y &key alpha)
  (declare (type fixnum sx sy width height x y))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (let ((src-get-fn (image-rgba-get-fn src-img :dx dx :dy dy))
          (dst-set-fn (image-rgba-blend-fn dst-img)))
      (declare (type image-rgba-get-fn src-get-fn)
               (type image-rgba-blend-fn dst-set-fn))
      (do-copy-image src-img sx sy width height dst-img x y (i j)
        (multiple-value-bind (red green blue a)
            (funcall src-get-fn i j)
          (funcall dst-set-fn i j red green blue (octet-mult a alpha)))))))

(defmethod blend-image ((src-img image-mixin) sx sy width height
                        (dst-img rgb-image-mixin) x y &key alpha)
  (declare (type fixnum sx sy width height x y))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (let ((src-get-fn (image-rgb-get-fn src-img :dx dx :dy dy))
          (dst-set-fn (image-rgb-blend-fn dst-img)))
      (declare (type image-rgb-get-fn src-get-fn)
               (type image-rgb-blend-fn dst-set-fn))
      (do-copy-image src-img sx sy width height dst-img x y (i j)
        (multiple-value-bind (red green blue)
            (funcall src-get-fn i j)
          (funcall dst-set-fn i j red green blue alpha))))))

(defmethod blend-image ((src-img rgba-image-mixin) sx sy width height
                        (dst-img rgb-image-mixin) x y &key alpha)
  (declare (type fixnum sx sy width height x y))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (let ((src-get-fn (image-rgba-get-fn src-img :dx dx :dy dy))
          (dst-set-fn (image-rgb-blend-fn dst-img)))
      (declare (type image-rgba-get-fn src-get-fn)
               (type image-rgb-blend-fn dst-set-fn))
      (do-copy-image src-img sx sy width height dst-img x y (i j)
        (multiple-value-bind (red green blue a)
            (funcall src-get-fn i j)
          (funcall dst-set-fn i j red green blue (octet-mult a alpha)))))))

(defmethod blend-image ((src-img image-mixin) sx sy width height
                        (dst-img gray-image-mixin) x y &key alpha)
  (declare (type fixnum sx sy width height x y))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (let ((src-get-fn (image-gray-get-fn src-img :dx dx :dy dy))
          (dst-set-fn (image-gray-blend-fn dst-img)))
      (declare (type image-gray-get-fn src-get-fn)
               (type image-gray-blend-fn dst-set-fn))
      (do-copy-image src-img sx sy width height dst-img x y (i j)
        (multiple-value-bind (gray)
            (funcall src-get-fn i j)
          (funcall dst-set-fn i j gray alpha))))))

(defmethod blend-image ((src-img rgba-image-mixin) sx sy width height
                        (dst-img gray-image-mixin) x y &key alpha)
  (declare (type fixnum sx sy width height x y))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (let ((src-get-fn (image-gray-alpha-get-fn src-img :dx dx :dy dy))
          (dst-set-fn (image-gray-blend-fn dst-img)))
      (declare (type image-gray-get-fn src-get-fn)
               (type image-gray-blend-fn dst-set-fn))
      (do-copy-image src-img sx sy width height dst-img x y (i j)
        (multiple-value-bind (gray a)
            (funcall src-get-fn i j)
          (funcall dst-set-fn i j gray (octet-mult alpha a)))))))

;;;
;;; coerce
;;;
(defmethod coerce-image ((image basic-image) image-class)
  (if (typep image image-class)
      image
      (clone-image image image-class)))

;;;
;;; clone
;;;
(defmethod clone-image ((image basic-image) image-class)
  (let ((dest (make-instance image-class
                             :width (image-width image)
                             :height (image-height image))))
    (copy-image image 0 0 (image-width image) (image-height image)
                dest 0 0)
    dest))
