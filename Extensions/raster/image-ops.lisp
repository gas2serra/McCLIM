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

(defmacro def-copy-image (src-image-class dst-image-class src-fn dst-fn channels)
  `(defmethod copy-image ((src-img ,src-image-class) sx sy width height
                          (dst-img ,dst-image-class) x y)
     (declare (type fixnum sx sy width height x y))
     (let ((dy (- sy y))
           (dx (- sx x)))
       (let ((src-get-fn (,src-fn src-img :dx dx :dy dy))
             (dst-set-fn (,dst-fn dst-img)))
         (declare (type ,src-fn src-get-fn)
                  (type ,dst-fn dst-set-fn))
         (do-copy-image src-img sx sy width height dst-img x y (i j)
           ,(case channels
              (4
               `(multiple-value-bind (red green blue alpha)
                    (funcall src-get-fn i j)
                  (funcall dst-set-fn i j red green blue alpha)))
              (3 `(multiple-value-bind (red green blue)
                      (funcall src-get-fn i j)
                    (funcall dst-set-fn i j red green blue)))
              (1 
               `(funcall dst-set-fn i j (funcall src-get-fn i j)))))))))

(def-copy-image image-mixin rgba-image-mixin image-rgba-get-fn image-rgba-set-fn 4)
(def-copy-image image-mixin rgb-image-mixin image-rgb-get-fn image-rgb-set-fn 3)
(def-copy-image image-mixin gray-image-mixin image-gray-get-fn image-gray-set-fn 1)
(def-copy-image alpha-channel-image-mixin alpha-channel-image-mixin
  image-alpha-get-fn image-alpha-set-fn 1)
(def-copy-image stencil-image-mixin gray-image-mixin image-alpha-get-fn image-gray-set-fn 1)
(def-copy-image gray-image-mixin stencil-image-mixin image-gray-get-fn image-alpha-set-fn 1)

;; fast copy

(defmacro def-fast-copy-image (src-image-class dst-image-class get-code set-code channels)
  `(defmethod copy-image ((src-img ,src-image-class) sx sy width height
                          (dst-img ,dst-image-class) x y)
     (declare (type fixnum sx sy width height x y))
     (let ((src-pixels (image-pixels src-img))
           (dst-pixels (image-pixels dst-img)))
       (declare (type ,(image-pixels-type src-image-class) src-pixels)
                (type ,(image-pixels-type dst-image-class) dst-pixels))
       (let ((dy (- sy y))
             (dx (- sx x)))
         ,(case channels
            (4
             `(macrolet ((get-code-m (i j)
                           (,get-code ',src-image-class `src-pixels i j))
                         (set-code-m (i j r g b a)
                           (,set-code ',dst-image-class `dst-pixels i j r g b a)))
                (declare (type fixnum dx dy))
                (do-copy-image src-img sx sy width height dst-img x y (i j)
                  (multiple-value-bind (red green blue alpha)
                      (get-code-m (+ i dx) (+ j dy))
                    (set-code-m i j red green blue alpha)))))
            (3
             `(macrolet ((get-code-m (i j)
                           (,get-code ',src-image-class `src-pixels i j))
                         (set-code-m (i j r g b)
                           (,set-code ',dst-image-class `dst-pixels i j r g b)))
                (declare (type fixnum dx dy))
                (do-copy-image src-img sx sy width height dst-img x y (i j)
                  (multiple-value-bind (red green blue)
                      (get-code-m (+ i dx) (+ j dy))
                    (set-code-m i j red green blue)))))
            (1
             `(macrolet ((get-code-m (i j)
                           (,get-code ',src-image-class `src-pixels i j))
                         (set-code-m (i j v)
                           (,set-code ',dst-image-class `dst-pixels i j v)))
                (declare (type fixnum dx dy))
                (do-copy-image src-img sx sy width height dst-img x y (i j)
                  (set-code-m i j (get-code-m (+ i dx) (+ j dy)))))))))))

(defmacro def-fast-rgba-copy-image (src-image-class dst-image-class)
  `(def-fast-copy-image ,src-image-class ,dst-image-class
     image-rgba-get-code image-rgba-set-code 4))

(defmacro def-fast-rgb-copy-image (src-image-class dst-image-class)
  `(def-fast-copy-image ,src-image-class ,dst-image-class
     image-rgb-get-code image-rgb-set-code 3))

(defmacro def-fast-gray-copy-image (src-image-class dst-image-class)
  `(def-fast-copy-image ,src-image-class ,dst-image-class
     image-gray-get-code image-gray-set-code 1))

(defmacro def-fast-alpha-copy-image (src-image-class dst-image-class)
  `(def-fast-copy-image ,src-image-class ,dst-image-class
     image-alpha-get-code image-alpha-set-code 1))

(defmacro def-fast-gray->alpha-copy-image (src-image-class dst-image-class)
  `(def-fast-copy-image ,src-image-class ,dst-image-class
     image-gray-get-code image-alpha-set-code 1))

(defmacro def-fast-alpha->gray-copy-image (src-image-class dst-image-class)
  `(def-fast-copy-image ,src-image-class ,dst-image-class
     image-alpha-get-code image-gray-set-code 1))

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
