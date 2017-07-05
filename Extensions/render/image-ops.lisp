(in-package :mcclim-render)

(defgeneric fill-image (image design stencil &key x y width height 
                                               stencil-dx stencil-dy))

(defgeneric coerce-image (image image-class))

(defgeneric write-image (image destination &key type quality))

;;;
;;; copy functions
;;;



;;;
;;; fill functions
;;;

(defmacro make-fill-image-with-stencil (image-class stencil-class)
  `(progn
     (defmethod fill-image ((image ,image-class) rgba-design (stencil ,stencil-class)
                            &key (x 0) (y 0) (width 0) (height 0) (stencil-dx 0) (stencil-dy 0))
       (declare (type fixnum x y width height stencil-dx stencil-dy))
       (let ((pixels (image-pixels image))
             (stencil-pixels (image-pixels stencil)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type ,(image-pixels-type stencil-class) stencil-pixels))
         (when (and (> width 0) (> height 0))
           (let ((max-y (+ y height -1))
                 (max-x (+ x width -1)))
             (let ((source-fn (make-pixeled-rgba-octets-fn rgba-design)))
               (declare (type pixeled-design-fn source-fn))
               (loop for j from y to max-y do
                    (loop for i from x to max-x do
                         (multiple-value-bind (red green blue alpha)
                             (funcall source-fn i j)
                           (let* ((alpha-ste ,(make-get-alpha-octet-code stencil-class 'stencil-pixels `(+ stencil-dx i) `(+ stencil-dy j)))
                                  (a (octet-mult alpha alpha-ste)))
                             (if (> a 250)
                                 ,(make-set-rgba-octets-code image-class 'pixels 'i 'j 'red 'green 'blue 'a)
                                 (multiple-value-bind (r.bg g.bg b.bg a.bg)
                                     ,(make-get-rgba-octets-code image-class 'pixels 'i 'j)
                                   (multiple-value-bind (red green blue alpha)
                                       (octet-blend-function red green blue a r.bg g.bg b.bg a.bg)
                                     ,(make-set-rgba-octets-code image-class 'pixels 'i 'j 'red 'green 'blue 'alpha)))))))))))
         (make-rectangle* x y (+ x width) (+ y height))))
     (defmethod fill-image ((image ,image-class) (rgba-design pixeled-uniform-design) (stencil ,stencil-class)
                                   &key (x 0) (y 0) (width 0) (height 0) (stencil-dx 0) (stencil-dy 0))
       (declare (type fixnum x y width height stencil-dx stencil-dy))
       (let ((pixels (image-pixels image))
             (stencil-pixels (image-pixels stencil)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type ,(image-pixels-type stencil-class) stencil-pixels))
         (when (and (> width 0) (> height 0))
           (let ((max-y (+ y height -1))
                 (max-x (+ x width -1)))
             (multiple-value-bind (red green blue alpha)
                 (values
                  (pixeled-uniform-design-red rgba-design)
                  (pixeled-uniform-design-green rgba-design)
                  (pixeled-uniform-design-blue rgba-design)
                  (pixeled-uniform-design-alpha rgba-design))
               (declare (type octet red green blue alpha))
               (loop for j from y to max-y do
                    (loop for i from x to max-x do
                         (let* ((alpha-ste ,(make-get-alpha-octet-code stencil-class 'stencil-pixels `(+ stencil-dx i) `(+ stencil-dy j)))
                                (a (octet-mult alpha alpha-ste)))
                           (if (> a 250)
                               ,(make-set-rgba-octets-code image-class 'pixels 'i 'j 'red 'green 'blue 'a)
                               (multiple-value-bind (r.bg g.bg b.bg a.bg)
                                   ,(make-get-rgba-octets-code image-class 'pixels 'i 'j)
                                 (multiple-value-bind (red green blue alpha)
                                     (octet-blend-function red green blue a r.bg g.bg b.bg a.bg)
                                   ,(make-set-rgba-octets-code image-class 'pixels 'i 'j 'red 'green 'blue 'alpha))))))))))
         (make-rectangle* x y (+ x width) (+ y height))))))

(defmacro make-fill-image-without-stencil (image-class)
  `(progn
     (defmethod fill-image ((image ,image-class) rgba-design (stencil (eql nil))
                                 &key (x 0) (y 0) (width 0) (height 0) (stencil-dx 0) (stencil-dy 0))
       (declare (type fixnum x y width height)
                (ignore stencil stencil-dx stencil-dy))
      (let ((pixels (image-pixels image)))
       (declare (type ,(image-pixels-type image-class) pixels))
       (when (and (> width 0) (> height 0))
         (let ((max-y (+ y height -1))
               (max-x (+ x width -1)))
           (let ((source-fn (make-pixeled-rgba-octets-fn rgba-design)))
             (declare (type pixeled-design-fn source-fn))
             (loop for j from y to max-y do
                  (loop for i from x to max-x do
                       (multiple-value-bind (red green blue alpha)
                           (funcall source-fn i j)
                         (if (> alpha 250)
                             ,(make-set-rgba-octets-code image-class 'pixels 'i 'j 'red 'green 'blue 'alpha)
                             (multiple-value-bind (r.bg g.bg b.bg a.bg)
                                 ,(make-get-rgba-octets-code image-class 'pixels 'i 'j)
                               (multiple-value-bind (red green blue alpha)
                                   (octet-blend-function red green blue alpha r.bg g.bg b.bg a.bg)
                                 ,(make-set-rgba-octets-code image-class 'pixels 'i 'j 'red 'green 'blue 'alpha)))))))))))
      (make-rectangle* x y (+ x width) (+ y height)))
     (defmethod fill-image ((image ,image-class) (rgba-design pixeled-uniform-design) (stencil (eql nil))
                                 &key (x 0) (y 0) (width 0) (height 0) (stencil-dx 0) (stencil-dy 0))
       (declare (type fixnum x y width height)
                (ignore stencil stencil-dx stencil-dy))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (when (and (> width 0) (> height 0))
           (let ((max-y (+ y height -1))
                 (max-x (+ x width -1)))
             (multiple-value-bind (red green blue alpha)
                 (values
                  (pixeled-uniform-design-red rgba-design)
                  (pixeled-uniform-design-green rgba-design)
                  (pixeled-uniform-design-blue rgba-design)
                  (pixeled-uniform-design-alpha rgba-design))
               (declare (type octet red green blue alpha))
               (loop for j from y to max-y do
                    (loop for i from x to max-x do
                         (if (> alpha 250)
                             ,(make-set-rgba-octets-code image-class 'pixels 'i 'j 'red 'green 'blue 'alpha)
                             (multiple-value-bind (r.bg g.bg b.bg a.bg)
                                 ,(make-get-rgba-octets-code image-class 'pixels 'i 'j)
                               (multiple-value-bind (red green blue alpha)
                                   (octet-blend-function red green blue alpha r.bg g.bg b.bg a.bg)
                                 ,(make-set-rgba-octets-code image-class 'pixels 'i 'j 'red 'green 'blue 'alpha))))))))))
       (make-rectangle* x y (+ x width) (+ y height)))))

;;;
;;; coerce functions
;;;


