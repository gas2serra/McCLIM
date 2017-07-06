(in-package :mcclim-render)

;;;
;;; Image operatios
;;;
(defmacro make-make-aa-render-draw-fn (image-class)
  `(progn
     (defmethod make-aa-render-draw-fn ((image ,image-class) clip-region pixeled-design)
       (let ((pixels (image-pixels image))
             (design-fn (make-pixeled-rgba-octets-fn pixeled-design)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type pixeled-design-fn design-fn))
         (if clip-region
             (lambda (x y alpha)
               (declare (type fixnum x y)
                        (type fixnum alpha))
               (when (clim:region-contains-position-p clip-region x y)
                 (setf alpha (min (abs alpha) 255))
                 (when (plusp alpha)
                   (multiple-value-bind (r.fg g.fg b.fg a.fg)
                       (funcall design-fn x y)
                     (if (> (octet-mult a.fg alpha) 250)
                         (multiple-value-bind (red green blue alpha)	  
                             (values r.fg g.fg b.fg 255)
                           ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha))
                         (multiple-value-bind (r.bg g.bg b.bg a.bg)
                             ,(image-rgba-get-code image-class 'pixels 'x 'y)
                           (multiple-value-bind (red green blue alpha)
                               (octet-blend-function r.fg g.fg b.fg (octet-mult a.fg alpha) r.bg g.bg b.bg a.bg)
                             ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha))))))))
             (lambda (x y alpha)
               (declare (type fixnum x y)
                        (type fixnum alpha))
               (setf alpha (min (abs alpha) 255))
               (when (plusp alpha)
                 (multiple-value-bind (r.fg g.fg b.fg a.fg)
                     (funcall design-fn x y)
                   (if (> (octet-mult a.fg alpha) 250)
                       (multiple-value-bind (red green blue alpha)	  
                           (values r.fg g.fg b.fg 255)
                         ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha))
                       (multiple-value-bind (r.bg g.bg b.bg a.bg)
                           ,(image-rgba-get-code image-class 'pixels 'x 'y)
                         (multiple-value-bind (red green blue alpha)
                             (octet-blend-function r.fg g.fg b.fg (octet-mult a.fg alpha) r.bg g.bg b.bg a.bg)
                           ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha))))))))))
     (defmethod make-aa-render-draw-fn ((image ,image-class) clip-region (pixeled-design pixeled-uniform-design))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (multiple-value-bind (r.fg g.fg b.fg a.fg)
             (values
              (pixeled-uniform-design-red pixeled-design)
              (pixeled-uniform-design-green pixeled-design)
              (pixeled-uniform-design-blue pixeled-design)
              (pixeled-uniform-design-alpha pixeled-design))
           (if clip-region
               (lambda (x y alpha)
                 (declare (type fixnum x y)
                          (type fixnum alpha))
                 (when (clim:region-contains-position-p clip-region x y)
                   (setf alpha (min (abs alpha) 255))
                   (when (plusp alpha)
                     (if (> (octet-mult a.fg alpha) 250)
                         (multiple-value-bind (red green blue alpha)	  
                             (values r.fg g.fg b.fg 255)
                           ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha))
                         (multiple-value-bind (r.bg g.bg b.bg a.bg)
                             ,(image-rgba-get-code image-class 'pixels 'x 'y)
                           (multiple-value-bind (red green blue alpha)
                               (octet-blend-function r.fg g.fg b.fg (octet-mult a.fg alpha) r.bg g.bg b.bg a.bg)
                             ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha)))))))
               (lambda (x y alpha)
                 (declare (type fixnum x y)
                          (type fixnum alpha))
                 (setf alpha (min (abs alpha) 255))
                 (when (plusp alpha)
                   (if (> (octet-mult a.fg alpha) 250)
                       (multiple-value-bind (red green blue alpha)	  
                           (values r.fg g.fg b.fg 255)
                         ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha))
                       (multiple-value-bind (r.bg g.bg b.bg a.bg)
                           ,(image-rgba-get-code image-class 'pixels 'x 'y)
                         (multiple-value-bind (red green blue alpha)
                             (octet-blend-function r.fg g.fg b.fg (octet-mult a.fg alpha) r.bg g.bg b.bg a.bg)
                           ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha))))))))))))

(defmacro make-make-aa-render-draw-span-fn (image-class)
  `(progn
     (defmethod make-aa-render-draw-span-fn ((image ,image-class) clip-region pixeled-design)
       (let ((pixels (image-pixels image))
             (design-fn (make-pixeled-rgba-octets-fn pixeled-design)))
         (declare (type ,(image-pixels-type image-class) pixels)
                  (type pixeled-design-fn design-fn))
         (if clip-region
             (lambda (x1 x2 y alpha)
               (declare (type fixnum x1 x2 y)
                        (type fixnum alpha))
               (loop for x from x1 below x2 do
                    (when (clim:region-contains-position-p clip-region x y)
                      (setf alpha (min (abs alpha) 255))
                      (when (plusp alpha)
                        (multiple-value-bind (r.fg g.fg b.fg a.fg)
                            (funcall design-fn x y)
                          (if (> (octet-mult a.fg alpha) 250)
                              (multiple-value-bind (red green blue alpha)	  
                                  (values r.fg g.fg b.fg 255)
                                ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha))
                              (multiple-value-bind (r.bg g.bg b.bg a.bg)
                                  ,(image-rgba-get-code image-class 'pixels 'x 'y)
                                (multiple-value-bind (red green blue alpha)
                                    (octet-blend-function r.fg g.fg b.fg (octet-mult a.fg alpha) r.bg g.bg b.bg a.bg)
                                  ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha)))))))))
             (lambda (x1 x2 y alpha)
               (declare (type fixnum x1 x2 y)
                        (type fixnum alpha))
               (setf alpha (min (abs alpha) 255))
               (when (plusp alpha)
                 (loop for x from x1 below x2 do
                      (multiple-value-bind (r.fg g.fg b.fg a.fg)
                          (funcall design-fn x y)
                        (if (> (octet-mult a.fg alpha) 250)
                            (multiple-value-bind (red green blue alpha)	  
                                (values r.fg g.fg b.fg 255)
                              ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha))
                            (multiple-value-bind (r.bg g.bg b.bg a.bg)
                                ,(image-rgba-get-code image-class 'pixels 'x 'y)
                              (multiple-value-bind (red green blue alpha)
                                  (octet-blend-function r.fg g.fg b.fg (octet-mult a.fg alpha) r.bg g.bg b.bg a.bg)
                                ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha)))))))))))
     (defmethod make-aa-render-draw-span-fn ((image ,image-class) clip-region (pixeled-design pixeled-uniform-design))
       (let ((pixels (image-pixels image)))
         (declare (type ,(image-pixels-type image-class) pixels))
         (multiple-value-bind (r.fg g.fg b.fg a.fg)
             (values
              (pixeled-uniform-design-red pixeled-design)
              (pixeled-uniform-design-green pixeled-design)
              (pixeled-uniform-design-blue pixeled-design)
              (pixeled-uniform-design-alpha pixeled-design))
           (declare (type octet r.fg g.fg b.fg a.fg))
           (if clip-region
               (lambda (x1 x2 y alpha)
                 (declare (type fixnum x1 x2 y)
                          (type fixnum alpha))
                 (loop for x from x1 below x2 do
                      (when (clim:region-contains-position-p clip-region x y)
                        (setf alpha (min (abs alpha) 255))
                        (when (plusp alpha)
                          (if (> (octet-mult a.fg alpha) 250)
                              (multiple-value-bind (red green blue alpha)	  
                                  (values r.fg g.fg b.fg 255)
                                ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha))
                              (multiple-value-bind (r.bg g.bg b.bg a.bg)
                                  ,(image-rgba-get-code image-class 'pixels 'x 'y)
                                (multiple-value-bind (red green blue alpha)
                                    (octet-blend-function r.fg g.fg b.fg (octet-mult a.fg alpha) r.bg g.bg b.bg a.bg)
                                  ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha))))))))
               (lambda (x1 x2 y alpha)
                 (declare (type fixnum x1 x2 y)
                          (type fixnum alpha))
                 (setf alpha (min (abs alpha) 255))
                 (when (plusp alpha)
                   (loop for x from x1 below x2 do
                        (if (> (octet-mult a.fg alpha) 250)
                            (multiple-value-bind (red green blue alpha)	  
                                (values r.fg g.fg b.fg 255)
                              ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha))
                            (multiple-value-bind (r.bg g.bg b.bg a.bg)
                                ,(image-rgba-get-code image-class 'pixels 'x 'y)
                              (multiple-value-bind (red green blue alpha)
                                  (octet-blend-function r.fg g.fg b.fg (octet-mult a.fg alpha) r.bg g.bg b.bg a.bg)
                                ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha)))))))))))))

(defmacro make-make-aa-render-xor-draw-fn (image-class)
  `(defmethod make-aa-render-xor-draw-fn ((image ,image-class) clip-region pixeled-design)
     (let ((pixels (image-pixels image))
           (design-fn (make-pixeled-rgba-octets-fn pixeled-design)))
       (declare (type ,(image-pixels-type image-class) pixels)
                (type pixeled-design-fn design-fn))
       (if clip-region
           (lambda (x y alpha)
             (declare (type fixnum x y)
                      (type fixnum alpha))
             (when (clim:region-contains-position-p clip-region x y)
               (setf alpha (min (abs alpha) 255))
               (when (plusp alpha)
                 (multiple-value-bind (r.fg g.fg b.fg a.fg)
                     (funcall design-fn x y)
                   (multiple-value-bind (r.bg g.bg b.bg a.bg)
                       ,(image-rgba-get-code image-class 'pixels 'x 'y)
                     (multiple-value-bind (red green blue alpha)	  
                         (octet-blend-function (color-octet-xor r.bg r.fg) (color-octet-xor g.bg g.fg)
                                               (color-octet-xor b.bg b.fg) (octet-mult a.fg alpha)
                                               r.bg g.bg b.bg a.bg)
                       ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha)))))))
           (lambda (x y alpha)
             (declare (type fixnum x y)
                      (type fixnum alpha))
             (setf alpha (min (abs alpha) 255))
             (when (plusp alpha)
               (multiple-value-bind (r.fg g.fg b.fg a.fg)
                   (funcall design-fn x y)
                 (multiple-value-bind (r.bg g.bg b.bg a.bg)
                     ,(image-rgba-get-code image-class 'pixels 'x 'y)
                   (multiple-value-bind (red green blue alpha)	  
                       (octet-blend-function (color-octet-xor r.bg r.fg) (color-octet-xor g.bg g.fg)
                                             (color-octet-xor b.bg b.fg) (octet-mult a.fg alpha)
                                             r.bg g.bg b.bg a.bg)
                     ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha))))))))))

(defmacro make-make-aa-render-xor-draw-span-fn (image-class)
  `(defmethod make-aa-render-xor-draw-span-fn ((image ,image-class) clip-region pixeled-design)
     (let ((pixels (image-pixels image))
           (design-fn (make-pixeled-rgba-octets-fn pixeled-design)))
       (declare (type ,(image-pixels-type image-class) pixels)
                (type pixeled-design-fn design-fn))
       (if clip-region
           (lambda (x1 x2 y alpha)
             (declare (type fixnum x1 x2 y)
                      (type fixnum alpha))
             (loop for x from x1 below x2 do
                  (when (clim:region-contains-position-p clip-region x y)
                    (setf alpha (min (abs alpha) 255))
                    (when (plusp alpha)
                      (multiple-value-bind (r.fg g.fg b.fg a.fg)
                          (funcall design-fn x y)
                        (multiple-value-bind (r.bg g.bg b.bg a.bg)
                            ,(image-rgba-get-code image-class 'pixels 'x 'y)
                          (multiple-value-bind (red green blue alpha)	  
                              (octet-blend-function (color-octet-xor r.bg r.fg) (color-octet-xor g.bg g.fg)
                                                    (color-octet-xor b.bg b.fg) (octet-mult a.fg alpha)
                                                    r.bg g.bg b.bg a.bg)
                            ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha))))))))
           (lambda (x1 x2 y alpha)
             (declare (type fixnum x1 x2 y)
                      (type fixnum alpha))
             (setf alpha (min (abs alpha) 255))
             (when (plusp alpha)
               (loop for x from x1 below x2 do
                    (multiple-value-bind (r.fg g.fg b.fg a.fg)
                        (funcall design-fn x y)
                      (multiple-value-bind (r.bg g.bg b.bg a.bg)
                          ,(image-rgba-get-code image-class 'pixels 'x 'y)
                        (multiple-value-bind (red green blue alpha)	  
                            (octet-blend-function (color-octet-xor r.bg r.fg) (color-octet-xor g.bg g.fg)
                                                  (color-octet-xor b.bg b.fg) (octet-mult a.fg alpha)
                                                  r.bg g.bg b.bg a.bg)
                          ,(image-rgba-set-code image-class 'pixels 'x 'y 'red 'green 'blue 'alpha)))))))))))

;;;
;;; Stencil Operations
;;;

(defgeneric make-aa-render-alpha-draw-fn (image clip-region))
(defgeneric make-aa-render-alpha-draw-span-fn (image clip-region))

(defmacro make-make-aa-render-alpha-draw-fn (image-class)
  `(defmethod make-aa-render-alpha-draw-fn ((image ,image-class) clip-region)
     (let ((pixels (image-pixels image)))
       (declare (type ,(image-pixels-type image-class) pixels))
       (if clip-region
           (lambda (x y alpha)
             (declare (type fixnum x y)
                      (type fixnum alpha))
             (when (clim:region-contains-position-p clip-region x y)
               (setf alpha (min (abs alpha) 255))
               (when (plusp alpha)
                 ,(image-alpha-set-code image-class 'pixels 'x 'y 'alpha))))
           (lambda (x y alpha)
             (declare (type fixnum x y)
                      (type fixnum alpha))
             (setf alpha (min (abs alpha) 255))
             (when (plusp alpha)
               ,(image-alpha-set-code image-class 'pixels 'x 'y 'alpha)))))))

(defmacro make-make-aa-render-alpha-draw-span-fn (image-class)
  `(defmethod make-aa-render-alpha-draw-span-fn ((image ,image-class) clip-region)
     (let ((pixels (image-pixels image)))
       (declare (type ,(image-pixels-type image-class) pixels))
       (if clip-region
           (lambda (x1 x2 y alpha)
             (declare (type fixnum x1 x2 y)
                      (type fixnum alpha))
             (loop for x from x1 below x2 do
                  (when (clim:region-contains-position-p clip-region x y)
                    (setf alpha (min (abs alpha) 255))
                    (when (plusp alpha)
                      ,(image-alpha-set-code image-class 'pixels 'x 'y 'alpha)))))
           (lambda (x1 x2 y alpha)
             (declare (type fixnum x1 x2 y)
                      (type fixnum alpha))
             (setf alpha (min (abs alpha) 255))
             (loop for x from x1 below x2 do
                  (when (plusp alpha)
                    ,(image-alpha-set-code image-class 'pixels 'x 'y 'alpha))))))))



