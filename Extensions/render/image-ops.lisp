(in-package :mcclim-render-internals)

(declaim (optimize speed))

;;;
;;; Image I/O
;;;
(defmethod read-image (pathname &key format type medium)
  (unless format
    (setf format (intern (string-upcase
                          (pathname-type (pathname pathname)))
                         (find-package :keyword))))
  (if (image-format-read-supported-p format)
      (let ((image (funcall (gethash format *image-file-readers*)
                            pathname)))
        (if (or type medium)
            (coerce-image image type medium)
            image))
      (error "image format not supproted, yet")))

(defmethod write-image (image destination &key format quality)
  (declare (ignorable quality))
  (unless format
    (setf format (intern (string-upcase
                          (pathname-type (pathname destination)))
                         (find-package :keyword))))
  (if (image-format-write-supported-p format)
      (funcall (gethash format *image-file-writer*)
               image destination)
      (error "image format not supproted, yet")))

;;;
;;; copy functions
;;;
(defmacro do-copy-image (src-img sx sy width height dst-img x y (i-var j-var) &body code)
  `(progn
     (let ((max-y (+ ,y ,height -1))
           (max-x (+ ,x ,width -1)))
       (declare (type fixnum max-x max-y))
       (flet ((copy-ff ()
                (loop for ,j-var fixnum from y to max-y do
                     (loop for ,i-var fixnum from x to max-x do
                          ,@code)))
              (copy-bf ()
                  (loop for ,j-var fixnum from y to max-y do
                       (loop for ,i-var fixnum from max-x downto x do
                            ,@code)))
              (copy-fb ()
                  (loop for ,j-var fixnum from max-y downto y do
                       (loop for ,i-var fixnum from x to max-x do
                            ,@code)))
                (copy-bb ()
                  (loop for ,j-var fixnum from max-y downto y do
                       (loop for ,i-var fixnum from max-x downto x do
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

(defmacro do-copy-image-fn (src-img src-fn sx sy width height dst-img dst-fn x y (i-var j-var)
                            &body code)
  `(progn
     (let ((dy (- ,sy ,y))
           (dx (- ,sx ,x)))
       (declare (type fixnum ,sx ,sy))
       (let ((src-get-fn (,src-fn src-img :dx dx :dy dy))
             (dst-set-fn (,dst-fn dst-img)))
         (declare (type ,src-fn src-get-fn)
                  (type ,dst-fn dst-set-fn))
         (do-copy-image ,src-img ,sx ,sy ,width ,height ,dst-img ,x ,y (,i-var ,j-var)
                        ,@code)))))

(defmethod copy-image :around ((src-img image-mixin) sx sy width height
                               (dst-img image-mixin) x y)
  (assert (and (>= sx 0) (>= sy 0) (>= x 0) (>= y 0) (>= width 0) (>= height 0)
               (<= (+ sx width) (image-width src-img)) (<= (+ sy height) (image-height src-img))
               (<= (+ x width) (image-width dst-img)) (<= (+ y height) (image-height dst-img))))
  (call-next-method src-img (round sx) (round sy) (round width) (round height)
                    dst-img (round x) (round y)))

(defmacro def-copy-image (src-image-class dst-image-class src-fn dst-fn channels)
  `(defmethod copy-image ((src-img ,src-image-class) sx sy width height
                          (dst-img ,dst-image-class) x y)
     (declare (type fixnum width height x y))
     (do-copy-image-fn src-img ,src-fn sx sy width height dst-img ,dst-fn x y (i j)
         ,(case channels
            (4
             `(multiple-value-bind (red green blue alpha)
                  (funcall src-get-fn i j)
                (funcall dst-set-fn i j red green blue alpha)))
            (3 `(multiple-value-bind (red green blue)
                    (funcall src-get-fn i j)
                  (funcall dst-set-fn i j red green blue)))
            (1
             `(funcall dst-set-fn i j (funcall src-get-fn i j)))))))

(def-copy-image image-mixin rgba-image-mixin image-rgba-get-fn image-rgba-set-fn 4)
(def-copy-image image-mixin rgb-image-mixin image-rgb-get-fn image-rgb-set-fn 3)
(def-copy-image image-mixin gray-image-mixin image-gray-get-fn image-gray-set-fn 1)

;;;
;;; blend functions
;;;
(defmethod blend-image :around ((src-img image-mixin) sx sy width height
                                (dst-img image-mixin) x y &key (alpha 255))
  (assert (and (>= sx 0) (>= sy 0) (>= x 0) (>= y 0) (>= width 0) (>= height 0)
               (<= (+ sx width) (image-width src-img)) (<= (+ sy height) (image-height src-img))
               (<= (+ x width) (image-width dst-img)) (<= (+ y height) (image-height dst-img))))
  (call-next-method src-img (round sx) (round sy) (round width) (round height)
                    dst-img (round x) (round y) :alpha (round alpha)))

(defmacro def-blend-image (src-image-class dst-image-class src-fn dst-fn channels)
  `(defmethod blend-image ((src-img ,src-image-class) sx sy width height
                           (dst-img ,dst-image-class) x y &key (alpha 255))
     (declare (type fixnum width height x y))
     (do-copy-image-fn src-img ,src-fn sx sy width height dst-img ,dst-fn x y (i j)
       ,(case channels
          (4
           `(multiple-value-bind (red green blue a)
                (funcall src-get-fn i j)
              (funcall dst-set-fn i j red green blue (octet-mult a alpha))))
          (3 `(multiple-value-bind (red green blue)
                  (funcall src-get-fn i j)
                (funcall dst-set-fn i j red green blue alpha)))
          (2
           `(multiple-value-bind (gray a)
                (funcall src-get-fn i j)
              (funcall dst-set-fn i j gray (octet-mult alpha a))))
          (1
           `(funcall dst-set-fn i j (funcall src-get-fn i j) alpha))))))

(def-blend-image image-mixin rgba-image-mixin
  image-rgba-get-fn image-rgba-blend-fn 4)
(def-blend-image image-mixin rgb-image-mixin
  image-rgb-get-fn image-rgb-blend-fn 3)
(def-blend-image rgba-image-mixin rgb-image-mixin
  image-rgba-get-fn image-rgb-blend-fn 4)
(def-blend-image image-mixin gray-image-mixin
  image-gray-get-fn image-gray-blend-fn 1)
(def-blend-image rgba-image-mixin gray-image-mixin
  image-gray-alpha-get-fn image-gray-blend-fn 2)

;;;
;;; make
;;;
(defmethod make-image :around (medium type width height)
  (call-next-method medium type (round width) (round height)))

(defmethod make-image ((medium (eql :default)) type width height)
  (make-image *default-image-medium* type width height))

;;;
;;; coerce
;;;
(defmethod coerce-image :around ((image image-mixin) type &optional medium)
  (if (eql type :auto)
      (call-next-method image (image-type image) (or medium (image-medium image)))
      (call-next-method image type (or medium (image-medium image)))))

(defmethod coerce-image ((image image-mixin) type &optional medium)
  (if (and (eql (image-type image) type)
           (eql (image-medium image) medium))
      image
      (clone-image image type medium)))

;;;
;;; clone
;;;
(defmethod clone-image :around ((image image-mixin) type &optional medium)
  (if (eql type :auto)
      (call-next-method image (image-type image) (or medium (image-medium image)))
      (call-next-method image type (or medium (image-medium image)))))

(defmethod clone-image ((image image-mixin) type &optional medium)
  (let ((dest (make-image medium
                          type
                          (image-width image)
                          (image-height image))))
    (copy-image image 0 0 (image-width image) (image-height image)
                dest 0 0)
    dest))

;;;
;;; crop
;;;
(defmethod crop-image :around ((image image-mixin) sx sy width height &optional type medium)
  (call-next-method image sx sy width height (or type (image-type image))
                    (or medium (image-medium image))))

(defmethod crop-image ((image image-mixin) sx sy width height &optional type medium)
  (let ((dest (make-image medium
                          type
                          width
                          height)))
    (copy-image image sx sy width height dest 0 0)
    dest))

;;;
;;; alpha channel
;;;
(defmethod copy-alpha-channel :around ((src-img image-mixin) sx sy width height
                                       (dst-img image-mixin) x y)
  (assert (and (>= sx 0) (>= sy 0) (>= x 0) (>= y 0) (>= width 0) (>= height 0)
               (<= (+ sx width) (image-width src-img)) (<= (+ sy height) (image-height src-img))
               (<= (+ x width) (image-width dst-img)) (<= (+ y height) (image-height dst-img))))
  (call-next-method src-img (round sx) (round sy) (round width) (round height)
                    dst-img (round x) (round y)))

(defmacro def-copy-alpha-channel (src-image-class dst-image-class)
  `(defmethod copy-alpha-channel ((src-img ,src-image-class) sx sy width height
                                  (dst-img ,dst-image-class) x y)
     (declare (type fixnum width height x y))
     (do-copy-image-fn src-img image-alpha-get-fn sx sy width height
         dst-img image-alpha-set-fn x y (i j)
       (funcall dst-set-fn i j (funcall src-get-fn i j)))))

(def-copy-alpha-channel image-mixin image-mixin)

(defmethod coerce-alpha-channel :around ((image image-mixin) &optional (type :gray) medium)
  (call-next-method image type (or medium (image-medium image))))

(defmethod coerce-alpha-channel ((image image-mixin) &optional type medium)
  (if (and (eql (image-type image) type)
           (eql (image-medium image) medium))
      image
      (clone-alpha-channel image type medium)))

(defmethod clone-alpha-channel :around ((image image-mixin) &optional (type :gray) medium)
  (call-next-method image type (or medium (image-medium image))))

(defmethod clone-alpha-channel ((image image-mixin) &optional type medium)
  (let ((dest (make-image medium
                          type
                          (image-width image)
                          (image-height image))))
    (copy-alpha-channel image 0 0 (image-width image) (image-height image)
                        dest 0 0)
    dest))

;;;
;;; Fill
;;;
(defparameter *alpha-epsilon* 10)
(defmethod fill-image :around ((image image-mixin) design stencil
                               &key (x 0) (y 0)
                                 (width (image-width image))
                                 (height (image-height image))
                                 (stencil-dx 0) (stencil-dy 0))
  (call-next-method image design stencil :x (round x) :y (round y)
                    :width (round width) :height (round height)
                    :stencil-dx (and stencil-dx (round stencil-dx))
                    :stencil-dy (and stencil-dy (round stencil-dy)))
  (make-rectangle* (round x) (round y) (round (+ x width)) (round (+ y height))))

(defgeneric fill-image-fn (image design))
(deftype fill-image-fn () '(function (fixnum fixnum octet)))

(defmethod fill-image-fn ((image rgba-image-mixin) design)
  (let ((src-fn (pixeled-design-rgba-get-fn design))
        (set-fn (image-rgba-set-fn image))
        (blend-fn (image-rgba-blend-fn image)))
    (declare (type pixeled-design-fn src-fn)
             (type image-rgba-set-fn set-fn)
             (type image-rgba-blend-fn blend-fn))
    (lambda (x y salpha)
      (multiple-value-bind (red green blue alpha)
          (funcall src-fn x y)
        (declare (type fixnum red green blue alpha *alpha-epsilon*))
        (setf alpha (octet-mult salpha alpha))
        (when (> alpha *alpha-epsilon*)
          (funcall blend-fn x y red green blue alpha))))))

(defmethod fill-image-fn ((image rgb-image-mixin) design)
  (let ((src-fn (pixeled-design-rgba-get-fn design))
        (set-fn (image-rgb-set-fn image))
        (blend-fn (image-rgb-blend-fn image)))
    (declare (type pixeled-design-fn src-fn)
             (type image-rgb-set-fn set-fn)
             (type image-rgb-blend-fn blend-fn))
    (lambda (x y salpha)
      (multiple-value-bind (red green blue alpha)
          (funcall src-fn x y)
        (declare (type fixnum red green blue alpha *alpha-epsilon*))
        (setf alpha (octet-mult salpha alpha))
        (cond ((> alpha (- 255 *alpha-epsilon*))
               (funcall set-fn x y red green blue))
              ((> alpha *alpha-epsilon*)
               (funcall blend-fn x y red green blue alpha)))))))

(defmethod fill-image-fn ((image rgb-image-mixin) (design pixeled-uniform-design))
  (let ((set-fn (image-rgb-set-fn image))
        (blend-fn (image-rgb-blend-fn image)))
    (declare (type image-rgb-set-fn set-fn)
             (type image-rgb-blend-fn blend-fn))
    (multiple-value-bind (red green blue alpha)
        (values
         (pixeled-uniform-design-red design)
         (pixeled-uniform-design-green design)
         (pixeled-uniform-design-blue design)
         (pixeled-uniform-design-alpha design))
      (let ((dalpha alpha))
        (declare (type fixnum red green blue alpha *alpha-epsilon*))
        (lambda (x y salpha)
          ;;(format *debug-io* "~A ~A~%" alpha salpha)
          (let ((alpha (octet-mult salpha dalpha)))
            (cond ((> alpha (- 255 *alpha-epsilon*))
                   (funcall set-fn x y red green blue))
                  ((> alpha *alpha-epsilon*)
                   (funcall blend-fn x y red green blue alpha)))))))))

(defmethod fill-image-fn ((image gray-image-mixin) design)
  (let ((src-fn (pixeled-design-rgba-get-fn design))
        (set-fn (image-gray-set-fn image))
        (blend-fn (image-gray-blend-fn image)))
    (declare (type pixeled-design-fn src-fn)
             (type image-gray-set-fn set-fn)
             (type image-gray-blend-fn blend-fn))
    (lambda (x y salpha)
      (multiple-value-bind (red green blue alpha)
          (funcall src-fn x y)
        (declare (type fixnum red green blue alpha *alpha-epsilon*))
        (setf alpha (octet-mult salpha alpha))
        (cond ((> alpha (- 255 *alpha-epsilon*))
               (funcall set-fn x y (rgb->gray red green blue)))
              ((> alpha *alpha-epsilon*)
               (funcall blend-fn x y (rgb->gray red green blue) alpha)))))))

(defmethod fill-image ((image image-mixin) design (stencil (eql nil))
                       &key x y width height stencil-dx stencil-dy)
  (declare (ignore stencil stencil-dx stencil-dy)
           (type fixnum x y width height))
  (let ((fn (fill-image-fn image design)))
    (declare (type  fill-image-fn fn))
    (let ((max-x (+ x width -1))
          (max-y (+ y height -1)))
      (declare (type fixnum max-x max-y))
      (loop for j-var from y to max-y do
           (loop for i-var from x to max-x do
                (funcall fn i-var j-var 255))))))

(defmethod fill-image ((image image-mixin) design (stencil gray-image-mixin)
                       &key x y width height stencil-dx stencil-dy)
  (declare (type fixnum x y width height))
  (let ((fn (fill-image-fn image design))
        (stencil-fn (image-alpha-get-fn stencil :dx stencil-dx :dy stencil-dy)))
    (declare (type  fill-image-fn fn)
             (type image-alpha-get-fn stencil-fn))
    (let ((max-x (+ x width -1))
          (max-y (+ y height -1)))
      (declare (type fixnum max-x max-y))
      (loop for j-var from y to max-y do
           (loop for i-var from x to max-x do
                (let ((alpha (funcall stencil-fn i-var j-var)))
                  (funcall fn i-var j-var alpha)))))))

