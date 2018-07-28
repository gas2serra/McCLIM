(in-package :mcclim-render-internals)

;;;
;;; draw
;;;

(defmethod aa-render-draw-fn ((image image-mixin) clip-region pixeled-design)
  (let ((blend-fn (%image-blend-fn image))
        (image-type (image-type image))
        (design-fn (pixeled-design-rgba-get-fn pixeled-design)))
    (declare (type pixeled-design-fn design-fn))
    (lambda (x y alpha)
      (declare (type fixnum x y)
               (type fixnum alpha))
      (setf alpha (min (abs alpha) 255))
      (when (plusp alpha)
        (when (or (null clip-region)
                  (clim:region-contains-position-p clip-region x y))
          (multiple-value-bind (r.fg g.fg b.fg a.fg)
              (funcall design-fn x y)
            (%call-blend-image-fn image-type blend-fn
                                  x y r.fg g.fg b.fg (octet-mult a.fg alpha))))))))

(defmethod aa-render-draw-fn ((image image-mixin) clip-region
                              (pixeled-design pixeled-uniform-design))
  (let ((blend-fn (%image-blend-fn image))
        (image-type (image-type image)))
    (multiple-value-bind (r.fg g.fg b.fg a.fg)
        (values
         (pixeled-uniform-design-red pixeled-design)
         (pixeled-uniform-design-green pixeled-design)
         (pixeled-uniform-design-blue pixeled-design)
         (pixeled-uniform-design-alpha pixeled-design))
      (declare (type octet r.fg g.fg b.fg a.fg))
      (lambda (x y alpha)
        (declare (type fixnum x y)
                 (type fixnum alpha))
        (setf alpha (min (abs alpha) 255))
        (when (plusp alpha)
          (when (or (null clip-region)
                    (clim:region-contains-position-p clip-region x y))
            (%call-blend-image-fn image-type blend-fn
                                  x y r.fg g.fg b.fg (octet-mult a.fg alpha))))))))

(defmethod aa-render-draw-span-fn ((image image-mixin) clip-region pixeled-design)
  (let ((blend-fn (%image-blend-fn image))
        (image-type (image-type image))
        (design-fn (pixeled-design-rgba-get-fn pixeled-design)))
    (declare (type pixeled-design-fn design-fn))
    (lambda (start-x end-x y alpha)
      (declare (type fixnum start-x end-x y)
               (type fixnum alpha))
      (setf alpha (min (abs alpha) 255))
      (when (plusp alpha)
        (loop for x from start-x below end-x do
             (when (or (null clip-region)
                       (clim:region-contains-position-p clip-region x y))
               (multiple-value-bind (r.fg g.fg b.fg a.fg)
                   (funcall design-fn x y)
                 (%call-blend-image-fn image-type blend-fn
                                       x y r.fg g.fg b.fg (octet-mult a.fg alpha)))))))))

(defmethod aa-render-draw-span-fn ((image image-mixin) clip-region
                                   (pixeled-design pixeled-uniform-design))
  (let ((blend-fn (%image-blend-fn image))
        (blend-span-fn (%image-blend-span-fn image))
        (image-type (image-type image)))
    (multiple-value-bind (r.fg g.fg b.fg a.fg)
        (values
         (pixeled-uniform-design-red pixeled-design)
         (pixeled-uniform-design-green pixeled-design)
         (pixeled-uniform-design-blue pixeled-design)
         (pixeled-uniform-design-alpha pixeled-design))
      (declare (type octet r.fg g.fg b.fg a.fg))
      (if (null clip-region)
          (lambda (start-x end-x y alpha)
            (declare (type fixnum start-x end-x y)
                     (type fixnum alpha))
            (setf alpha (min (abs alpha) 255))
            (when (plusp alpha)
              (%call-blend-image-span-fn image-type blend-span-fn
                                         start-x y end-x y r.fg g.fg b.fg (octet-mult a.fg alpha))))
          (lambda (start-x end-x y alpha)
            (declare (type fixnum start-x end-x y)
                     (type fixnum alpha))
            (setf alpha (min (abs alpha) 255))
            (when (plusp alpha)
              (loop for x from start-x below end-x do
                   (when (or (null clip-region)
                             (clim:region-contains-position-p clip-region x y))
                     (%call-blend-image-fn image-type blend-fn
                                           x y r.fg g.fg b.fg (octet-mult a.fg alpha))))))))))

;;;
;;;
;;;

(defmethod aa-render-xor-draw-fn ((image image-mixin) clip-region
                                  (pixeled-design pixeled-flipping-design))
  (let ((blend-fn (%image-xor-blend-fn image))
        (image-type (image-type image))
        (design-fn (pixeled-design-rgba-get-fn pixeled-design)))
    (declare (type pixeled-design-fn design-fn))
    (lambda (x y alpha)
      (declare (type fixnum x y)
               (type fixnum alpha))
      (setf alpha (min (abs alpha) 255))
      (when (plusp alpha)
        (when (or (null clip-region)
                  (clim:region-contains-position-p clip-region x y))
          (multiple-value-bind (r.fg g.fg b.fg a.fg)
              (funcall design-fn x y)
            (%call-blend-image-fn image-type blend-fn
                                  x y r.fg g.fg b.fg (octet-mult a.fg alpha))))))))

(defmethod aa-render-xor-draw-span-fn ((image image-mixin) clip-region
                                       (pixeled-design pixeled-flipping-design))
  (let ((blend-fn (%image-xor-blend-fn image))
        (image-type (image-type image))
        (design-fn (pixeled-design-rgba-get-fn pixeled-design)))
    (declare (type pixeled-design-fn design-fn))
    (lambda (start-x end-x y alpha)
      (declare (type fixnum start-x end-x y)
               (type fixnum alpha))
      (setf alpha (min (abs alpha) 255))
      (when (plusp alpha)
        (loop for x from start-x below end-x do
             (when (or (null clip-region)
                       (clim:region-contains-position-p clip-region x y))
               (multiple-value-bind (r.fg g.fg b.fg a.fg)
                   (funcall design-fn x y)
                 (%call-blend-image-fn image-type blend-fn
                                           x y r.fg g.fg b.fg (octet-mult a.fg alpha)))))))))
;;;
;;; alpha draw
;;;

(defmethod aa-render-alpha-draw-fn ((image gray-image-mixin) clip-region)
  (let ((set-fn (image-gray-set-fn image)))
    (declare (type image-gray-set-fn set-fn))
    (lambda (x y alpha)
      (declare (type fixnum x y)
               (type fixnum alpha))
      (setf alpha (min (abs alpha) 255))
      (when (plusp alpha)
        (when (or (null clip-region)
                  (clim:region-contains-position-p clip-region x y))
          (funcall set-fn x y alpha))))))

(defmethod aa-render-alpha-draw-span-fn ((image gray-image-mixin) clip-region)
  (let ((set-fn (image-gray-set-fn image))
        (set-span-fn (image-gray-set-span-fn image)))
    (declare (type image-gray-set-fn set-fn))
    (lambda (x1 x2 y alpha)
      (declare (type fixnum x1 x2 y)
               (type fixnum alpha))
      (setf alpha (min (abs alpha) 255))
      (when (plusp alpha)
        (if (null clip-region)
            (funcall set-span-fn x1 y x2 y alpha)
            (loop for x from x1 below x2 do
                 (clim:region-contains-position-p clip-region x y)
                 (funcall set-fn x y alpha)))))))
