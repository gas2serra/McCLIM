(in-package :mcclim-render-internals)

#+nil (declaim (optimize speed))

;;;
;;; make
;;;
(defvar *default-image-medium* :two-dim-array)

(defmethod make-image :around (medium type width height)
  ;; TO FIX ROUNDING
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
  (let ((dest (make-image medium type (image-width image) (image-height image))))
    (copy-image image 0 0 (image-width image) (image-height image) dest 0 0)
    dest))

;;;
;;; copy
;;;
(defmethod copy-image :around ((src-img image-mixin) sx sy width height
                               (dst-img image-mixin) x y)
  ;; TO FIX ROUNDING
  (assert (and (>= sx 0) (>= sy 0) (>= x 0) (>= y 0) (>= width 0) (>= height 0)
               (<= (+ sx width) (image-width src-img)) (<= (+ sy height) (image-height src-img))
               (<= (+ x width) (image-width dst-img)) (<= (+ y height) (image-height dst-img))))
  (call-next-method src-img (round sx) (round sy) (round width) (round height)
                    dst-img (round x) (round y)))

(defmethod copy-image ((src-img image-mixin) sx sy width height
                       (dst-img image-mixin) x y)
  (declare (type fixnum x y sx sy width height))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (declare (type fixnum dx dy))
    (let ((set-fn (%image-set-fn dst-img))
          (get-fn (%image-get-fn src-img :dx dx :dy dy))
          (src-image-type (image-type src-img))
          (dst-image-type (image-type dst-img)))
      (do-copy-image src-img sx sy width height dst-img x y (i j)
        (%call-image-copy-fn src-image-type dst-image-type get-fn set-fn i j)))))

;;;
;;; blend
;;;
(defmethod blend-image :around ((src-img image-mixin) sx sy width height
                                (dst-img image-mixin) x y &key (alpha 255))
  ;; TO FIX ROUNDING
  (assert (and (>= sx 0) (>= sy 0) (>= x 0) (>= y 0) (>= width 0) (>= height 0)
               (<= (+ sx width) (image-width src-img)) (<= (+ sy height) (image-height src-img))
               (<= (+ x width) (image-width dst-img)) (<= (+ y height) (image-height dst-img))))
  (call-next-method src-img (round sx) (round sy) (round width) (round height)
                    dst-img (round x) (round y) :alpha (round alpha)))

(defmethod blend-image ((src-img image-mixin) sx sy width height
                        (dst-img image-mixin) x y &key (alpha 255))
  (declare (type fixnum x y sx sy width height))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (declare (type fixnum dx dy))
    (let ((blend-fn (%image-blend-fn dst-img))
          (get-fn (%image-get-fn src-img :dx dx :dy dy))
          (src-image-type (image-type src-img))
          (dst-image-type (image-type dst-img)))
       (do-copy-image src-img sx sy width height dst-img x y (i j)
        (%call-image-blend-fn src-image-type dst-image-type get-fn blend-fn i j alpha)))))

;;;
;;; crop
;;;
(defmethod crop-image :around ((image image-mixin) sx sy width height &optional type medium)
  (call-next-method image sx sy width height (or type (image-type image))
                    (or medium (image-medium image))))

(defmethod crop-image ((image image-mixin) sx sy width height &optional type medium)
  (let ((dest (make-image medium type width height)))
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
  ;; TO FIX ROUNDING
  (call-next-method src-img (round sx) (round sy) (round width) (round height)
                    dst-img (round x) (round y)))

(defmethod copy-alpha-channel ((src-img image-mixin) sx sy width height
                               (dst-img image-mixin) x y)
  (declare (type fixnum x y sx sy width height))
  (let ((dy (- sy y))
        (dx (- sx x)))
    (declare (type fixnum dx dy))
    (let ((set-fn (image-alpha-set-fn dst-img))
          (get-fn (image-alpha-get-fn src-img :dx dx :dy dy)))
      (declare (type image-alpha-set-fn set-fn)
               (type image-alpha-get-fn get-fn))
      (do-copy-image src-img sx sy width height dst-img x y (i j)
        (funcall set-fn i j (funcall get-fn i j))))))

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
  (let ((dest (make-image medium type (image-width image) (image-height image))))
    (copy-alpha-channel image 0 0 (image-width image) (image-height image) dest 0 0)
    dest))

;;;
;;; I/O
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
;;; set
;;;
(defmethod set-image :around ((image image-mixin) design
                               &key (x 0) (y 0)
                                 (width (image-width image))
                                 (height (image-height image)))
  ;; TO FIX ROUNDING
  (call-next-method image design :x (round x) :y (round y)
                    :width (round width) :height (round height))
  (make-rectangle* (round x) (round y) (round (+ x width)) (round (+ y height))))

(defmethod set-image ((image image-mixin) design &key x y width height)
  (declare (type fixnum x y width height))
  (let ((set-fn (%image-set-fn image))
        (image-type (image-type image)))
    (let ((src-fn (pixeled-design-rgba-get-fn design)))
      (declare (type pixeled-design-fn src-fn))
      (let ((max-x (+ x width -1))
            (max-y (+ y height -1)))
        (declare (type fixnum max-x max-y))
        (loop for j-var from y to max-y do
             (loop for i-var from x to max-x do
                  (multiple-value-bind (red green blue alpha)
                      (funcall src-fn i-var j-var)
                    (%call-set-image-fn image-type set-fn 
                                        i-var j-var red green blue alpha))))))))

(defmethod set-image ((image image-mixin) (design pixeled-uniform-design)
                       &key x y width height)
  (declare (type fixnum x y width height))
  (let ((set-fn (%image-set-fn image))
        (set-span-fn (%image-set-span-fn image))
        (image-type (image-type image)))
    (let* ((test-region-p (not (region-equal (pixeled-design-region design) +everywhere+)))
           (src-fn (if test-region-p
                       (pixeled-design-rgba-get-fn design)
                       (lambda (x y) (declare (ignore x y)) (values 0 0 0 0)))))
      (declare (type pixeled-design-fn src-fn))
      (multiple-value-bind (red green blue alpha)
          (values
           (pixeled-uniform-design-red design)
           (pixeled-uniform-design-green design)
           (pixeled-uniform-design-blue design)
           (pixeled-uniform-design-alpha design))
        (let ((max-x (+ x width -1))
              (max-y (+ y height -1)))
          (declare (type fixnum max-x max-y))
          (if test-region-p
              (loop for j-var from y to max-y do
                   (loop for i-var from x to max-x do
                        (multiple-value-bind (r g b a)
                            (funcall src-fn i-var j-var)
                          (%call-set-image-fn image-type set-fn
                                              i-var j-var r g b a))))
              (%call-set-image-span-fn image-type set-span-fn
                                       x y max-x max-y red green blue alpha)))))))

;;;
;;; fill
;;;
(defmethod fill-image :around ((image image-mixin) design stencil
                               &key (x 0) (y 0)
                                 (width (image-width image))
                                 (height (image-height image))
                                 (stencil-dx 0) (stencil-dy 0))
  ;; TO FIX ROUNDING
  (call-next-method image design stencil :x (round x) :y (round y)
                    :width (round width) :height (round height)
                    :stencil-dx (and stencil-dx (round stencil-dx))
                    :stencil-dy (and stencil-dy (round stencil-dy)))
  (make-rectangle* (round x) (round y) (round (+ x width)) (round (+ y height))))

(defmethod fill-image ((image image-mixin) design stencil
                       &key x y width height stencil-dx stencil-dy)
  (declare (type fixnum x y width height))
  (let ((blend-fn (%image-blend-fn image))
        (image-type (image-type image)))
    (let ((src-fn (pixeled-design-rgba-get-fn design))
          (stencil-fn (if stencil
                          (image-alpha-get-fn stencil :dx stencil-dx :dy stencil-dy)
                          (lambda (x y) (declare (ignore x y)) 255))))
      (declare (type image-alpha-get-fn stencil-fn)
               (type pixeled-design-fn src-fn))
      (let ((max-x (+ x width -1))
            (max-y (+ y height -1)))
        (declare (type fixnum max-x max-y))
        (if stencil
            (loop for j-var from y to max-y do
                 (loop for i-var from x to max-x do
                      (multiple-value-bind (red green blue alpha)
                          (funcall src-fn i-var j-var)
                        (let ((salpha (funcall stencil-fn i-var j-var)))
                          (setf salpha (octet-mult salpha alpha))
                          (%call-fill-image-fn image-type blend-fn 
                                          i-var j-var red green blue salpha)))))
            (loop for j-var from y to max-y do
                 (loop for i-var from x to max-x do
                      (multiple-value-bind (red green blue alpha)
                          (funcall src-fn i-var j-var)
                        (%call-fill-image-fn image-type blend-fn 
                                        i-var j-var red green blue alpha)))))))))

(defmethod fill-image ((image image-mixin) (design pixeled-uniform-design) stencil
                       &key x y width height stencil-dx stencil-dy)
  (declare (type fixnum x y width height))
  (let ((blend-fn (%image-blend-fn image))
        (blend-span-fn (%image-blend-span-fn image))
        (image-type (image-type image)))
    (let* ((test-region-p (not (region-equal (pixeled-design-region design) +everywhere+)))
           (src-fn (if test-region-p
                       (pixeled-design-rgba-get-fn design)
                       (lambda (x y) (declare (ignore x y)) (values 0 0 0 0))))
           (stencil-fn (if stencil
                           (image-alpha-get-fn stencil :dx stencil-dx :dy stencil-dy)
                           (lambda (x y) (declare (ignore x y)) 255))))
      (declare (type image-alpha-get-fn stencil-fn)
               (type pixeled-design-fn src-fn))
      (multiple-value-bind (red green blue alpha)
          (values
           (pixeled-uniform-design-red design)
           (pixeled-uniform-design-green design)
           (pixeled-uniform-design-blue design)
           (pixeled-uniform-design-alpha design))
        (let ((max-x (+ x width -1))
              (max-y (+ y height -1)))
          (declare (type fixnum max-x max-y))
          (if test-region-p
              (loop for j-var from y to max-y do
                   (loop for i-var from x to max-x do
                        (let ((salpha (if stencil
                                          (funcall stencil-fn i-var j-var)
                                          255)))
                          (multiple-value-bind (r g b a)
                              (funcall src-fn i-var j-var)
                            (setf red r
                                  green g
                                  blue b
                                  alpha a))
                          (setf salpha (octet-mult salpha alpha))
                          (%call-fill-image-fn image-type blend-fn
                                          i-var j-var red green blue salpha))))
              (if stencil
                  (loop for j-var from y to max-y do
                       (loop for i-var from x to max-x do
                            (let ((salpha (funcall stencil-fn i-var j-var)))
                              (declare (type octet alpha salpha))
                              (setf salpha (octet-mult salpha alpha))
                              (%call-fill-image-fn image-type blend-fn 
                                              i-var j-var red green blue salpha))))
                  (%call-fill-image-span-fn image-type blend-span-fn
                                       x y max-x max-y red green blue alpha))))))))
