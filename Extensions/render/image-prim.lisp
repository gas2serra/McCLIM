(in-package :mcclim-render-internals)

#+nil (declaim (optimize speed))

;;;
;;; Utilities
;;;
(defparameter *alpha-epsilon* 10)

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


;;;
;;; generic primitives
;;;

;; -fn
(declaim (inline %image-get-fn %image-set-fn %image-set-span-fn %image-blend-fn))
(defun %image-get-fn (image &key (dx 0) (dy 0) region)
  (case (image-type image)
    (:rgba
     (image-rgba-get-fn image :dx dx :dy dy :region region))
    (:rgb
     (image-rgb-get-fn image :dx dx :dy dy :region region))
    (:gray
     (image-gray-get-fn image :dx dx :dy dy :region region))))
(defun %image-set-fn (image &key (dx 0) (dy 0))
  (case (image-type image)
    (:rgba
     (image-rgba-set-fn image :dx dx :dy dy))
    (:rgb
     (image-rgb-set-fn image :dx dx :dy dy))
    (:gray
     (image-gray-set-fn image :dx dx :dy dy))))
(defun %image-set-span-fn (image &key (dx 0) (dy 0))
  (case (image-type image)
    (:rgba
     (image-rgba-set-span-fn image :dx dx :dy dy))
    (:rgb
     (image-rgb-set-span-fn image :dx dx :dy dy))
    (:gray
     (image-gray-set-span-fn image :dx dx :dy dy))))
(defun %image-blend-fn (image &key (dx 0) (dy 0))
   (case (image-type image)
     (:rgba
      (image-rgba-blend-fn image :dx dx :dy dy))
     (:rgb
      (image-rgb-blend-fn image :dx dx :dy dy))
     (:gray
      (image-gray-blend-fn image :dx dx :dy dy))))
(defun %image-blend-span-fn (image &key (dx 0) (dy 0))
   (case (image-type image)
     (:rgba
      (image-rgba-blend-span-fn image :dx dx :dy dy))
     (:rgb
      (image-rgb-blend-span-fn image :dx dx :dy dy))
     (:gray
      (image-gray-blend-span-fn image :dx dx :dy dy))))
;; call-
(declaim (inline %call-image-rgba-get-fn))
(defun %call-image-rgba-get-fn (image-type get-fn x y)
  (declare (type fixnum x y))
  (multiple-value-bind (red green blue alpha)
      (case image-type
        (:rgba
         (let ()
           (declare (type image-rgba-get-fn get-fn))
           (funcall get-fn x y)))
        (:rgb
         (let ()
           (declare (type image-rgb-get-fn get-fn))
           (multiple-value-bind (r g b)
               (funcall get-fn x y)
             (rgb->rgba r g b))))
        (:gray
         (let ()
           (declare (type image-gray-get-fn get-fn))
           (multiple-value-bind (g)
               (funcall get-fn x y)
             (gray->rgba g)))))
    (values red green blue alpha)))
(declaim (inline %call-image-rgb-get-fn))
(defun %call-image-rgb-get-fn (image-type get-fn x y)
  (declare (type fixnum x y))
  (multiple-value-bind (red green blue)
      (case image-type
        (:rgba
         (let ()
           (declare (type image-rgba-get-fn get-fn))
           (funcall get-fn x y)))
        (:rgb
         (let ()
           (declare (type image-rgb-get-fn get-fn))
           (funcall get-fn x y)))
        (:gray
         (let ()
           (declare (type image-gray-get-fn get-fn))
           (multiple-value-bind (g)
               (funcall get-fn x y)
             (gray->rgb g)))))
    (values red green blue)))
(declaim (inline %call-image-gray-get-fn))
(defun %call-image-gray-get-fn (image-type get-fn x y)
  (declare (type fixnum x y))
  (multiple-value-bind (gray)
      (case image-type
        (:rgba
         (let ()
           (declare (type image-rgba-get-fn get-fn))
           (multiple-value-bind (red green blue alpha)
               (funcall get-fn x y)
             (rgba->gray red green blue alpha))))
        (:rgb
         (let ()
           (declare (type image-rgb-get-fn get-fn))
           (multiple-value-bind (red green blue)
               (funcall get-fn x y)
             (rgb->gray red green blue))))
        (:gray
         (let ()
           (declare (type image-gray-get-fn get-fn))
           (funcall get-fn x y))))
    (values gray)))

;; call-
(declaim (inline %call-image-copy-fn))
(defun %call-image-copy-fn (src-image-type dst-image-type get-fn set-fn x y)
  (declare (type fixnum x y)
           (type symbol src-image-type dst-image-type))
  (case dst-image-type
    (:rgba
     (let ()
       (declare (type image-rgba-set-fn set-fn))
       (multiple-value-bind (red green blue alpha)
           (%call-image-rgba-get-fn src-image-type get-fn x y)
         (funcall set-fn x y red green blue alpha))))
    (:rgb
     (let ()
       (declare (type image-rgb-set-fn set-fn))
       (multiple-value-bind (red green blue)
           (%call-image-rgb-get-fn src-image-type get-fn x y)
         (funcall set-fn x y red green blue))))
    (:gray
     (let ()
       (declare (type image-gray-set-fn set-fn))
       (multiple-value-bind (gray)
           (%call-image-gray-get-fn src-image-type get-fn x y)
       (funcall set-fn x y gray))))))

(declaim (inline %call-image-blend-fn))
(defun %call-image-blend-fn (src-image-type dst-image-type get-fn blend-fn x y salpha)
  (declare (type fixnum x y)
           (type octet salpha)
           (type symbol src-image-type dst-image-type))
  (case dst-image-type
    (:rgba
     (let ()
       (declare (type image-rgba-blend-fn blend-fn))
       (multiple-value-bind (red green blue alpha)
           (%call-image-rgba-get-fn src-image-type get-fn x y)
         (setf alpha (octet-mult salpha alpha))
         (when (> alpha *alpha-epsilon*)
           (funcall blend-fn x y red green blue alpha)))))
    (:rgb
     (let ()
       (declare (type image-rgb-blend-fn blend-fn))
       (multiple-value-bind (red green blue alpha)
           (%call-image-rgba-get-fn src-image-type get-fn x y)
         (setf alpha (octet-mult salpha alpha))
         (when (> alpha *alpha-epsilon*)
           (funcall blend-fn x y red green blue alpha)))))
    (:gray
     (let ()
       (declare (type image-gray-blend-fn blend-fn))
       (multiple-value-bind (red green blue alpha)
           (%call-image-rgba-get-fn src-image-type get-fn x y)
         (setf alpha (octet-mult salpha alpha)) 
         (when (> alpha *alpha-epsilon*)
           (funcall blend-fn x y (rgb->gray red green blue) alpha)))))))


(declaim (inline %fill-image-fn))
(defun %call-fill-image-fn (image-type blend-fn x y red green blue alpha)
  (declare (type octet red green blue alpha *alpha-epsilon*))
  (case image-type
    (:rgba
     (let ()
       (declare (type image-rgba-blend-fn blend-fn))
       (when (> alpha *alpha-epsilon*)
         (funcall blend-fn x y red green blue alpha))))
    (:rgb
     (let ()
       (declare (type image-rgb-blend-fn blend-fn))
       (when (> alpha *alpha-epsilon*)
         (funcall blend-fn x y red green blue alpha))))
    (:gray
     (let ()
       (declare (type image-gray-blend-fn blend-fn))
       (when (> alpha *alpha-epsilon*)
         (funcall blend-fn x y (rgb->gray red green blue) alpha))))))

(declaim (inline %call-fill-image-span-fn))
(defun %call-fill-image-span-fn (image-type blend-fn x1 y1 x2 y2 red green blue alpha)
  (declare (type octet red green blue alpha *alpha-epsilon*)
           (type fixnum x1 x2 y1 y2))
  (case image-type
    (:rgba
     (let ()
       (declare (type image-rgba-blend-span-fn blend-fn))
       (funcall blend-fn x1 y1 x2 y2 red green blue alpha)))
    (:rgb
     (let ()
       (declare (type image-rgb-blend-span-fn blend-fn))
       (funcall blend-fn x1 y1 x2 y2 red green blue alpha)))
    (:gray
     (let ()
       (declare (type image-gray-blend-span-fn blend-fn))
       (funcall blend-fn x1 y1 x2 y2 (rgb->gray red green blue) alpha)))))

;; set
(declaim (inline %set-image-fn))
(defun %call-set-image-fn (image-type set-fn x y red green blue alpha)
  (declare (type octet red green blue alpha *alpha-epsilon*))
  (case image-type
    (:rgba
     (let ()
       (declare (type image-rgba-set-fn set-fn))
       (funcall set-fn x y red green blue alpha)))
    (:rgb
     (let ()
       (declare (type image-rgb-set-fn set-fn))
       (multiple-value-bind (r g b)
           (rgba->rgb red green blue alpha)
         (funcall set-fn x y r g b))))
    (:gray
     (let ()
       (declare (type image-gray-set-fn set-fn))
       (funcall set-fn x y (rgba->gray red green blue alpha))))))

(declaim (inline %call-set-image-span-fn))
(defun %call-set-image-span-fn (image-type set-fn x1 y1 x2 y2 red green blue alpha)
  (declare (type octet red green blue alpha *alpha-epsilon*)
           (type fixnum x1 x2 y1 y2))
  (case image-type
    (:rgba
     (let ()
       (declare (type image-rgba-set-span-fn set-fn))
       (funcall set-fn x1 y1 x2 y2 red green blue alpha)))
    (:rgb
     (let ()
       (declare (type image-rgb-set-span-fn set-fn))
       (multiple-value-bind (r g b)
           (rgba->rgb red green blue alpha)
         (funcall set-fn x1 y1 x2 y2 r g b))))
    (:gray
     (let ()
       (declare (type image-gray-set-span-fn set-fn))
       (funcall set-fn x1 y1 x2 y2 (rgba->gray red green blue alpha))))))



