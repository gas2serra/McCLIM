(in-package :mcclim-render-internals)

#+nil (declaim (optimize speed))

;;;
;;; image manipulation functions
;;;

(deftype image-rgb-set-span-fn () '(function (fixnum fixnum fixnum fixnum octet octet octet)))
(defgeneric image-rgb-set-span-fn (image &key dx dy))

(defmethod image-rgb-set-span-fn ((image rgb-image-mixin) &key (dx 0) (dy 0))
  (let ((fn (image-rgb-set-fn image :dx dx :dy dy)))
    (declare (type image-rgb-set-fn fn))
    (lambda (x1 y1 x2 y2 red green blue)
      (declare (type fixnum x1 y1 x2 y2)
               (type octet red green blue))
      (loop for j from y1 to y2 do
                (loop for i from x1 to x2 do
                     (funcall fn i j red green blue))))))


;; rgba
(deftype image-gray-alpha-get-fn () '(function (fixnum fixnum) (values octet octet)))
(deftype image-rgba-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
;; rgb
(deftype image-rgb-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
(deftype image-rgb-xor-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
;; gray
(deftype image-gray-blend-fn () '(function (fixnum fixnum octet octet)))
;; alpha
(deftype image-alpha-get-fn () '(function (fixnum fixnum) octet))
(deftype image-alpha-set-fn () '(function (fixnum fixnum octet )))
(deftype image-alpha-blend-fn () '(function (fixnum fixnum octet octet)))

;; rgba
(defgeneric image-gray-alpha-get-fn (image &key dx dy region))
(defgeneric image-rgba-blend-fn (image &key dx dy))
;; rgb
(defgeneric image-rgb-blend-fn (image &key dx dy))
(defgeneric image-rgb-xor-blend-fn (image &key dx dy))
;; gray
(defgeneric image-gray-blend-fn (image &key dx dy))
;; alpha
(defgeneric image-alpha-get-fn (image &key dx dy region))
(defgeneric image-alpha-set-fn (image &key dx dy))
(defgeneric image-alpha-blend-fn (image &key dx dy))

;;;
;;; defaults
;;;

;; gray
(defmethod image-alpha-get-fn ((image gray-image-mixin) &key (dx 0) (dy 0) (region nil))
  (image-gray-get-fn image :dx dx :dy dy :region region))

(defmethod image-alpha-set-fn ((image gray-image-mixin) &key (dx 0) (dy 0))
  (image-gray-set-fn image :dx dx :dy dy))

(defmethod image-gray-blend-fn ((image gray-image-mixin) &key (dx 0) (dy 0))
  (let ((sfn (image-gray-set-fn image :dx dx :dy dy))
        (gfn (image-gray-get-fn image :dx dx :dy dy)))
    (declare (type image-gray-get-fn gfn)
             (type image-gray-set-fn sfn))
    (lambda (x y gray alpha)
      (declare (type fixnum x y gray alpha))
      (let ((g (funcall gfn x y)))
        (funcall sfn x y (octet-gray-blend-function gray alpha g))))))

;; rgb
(defmethod image-alpha-get-fn ((image rgb-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-rgb-get-fn image :dx dx :dy dy :region region)))
    (declare (type image-rgb-get-fn fn))
    (lambda (x y)
      (declare (type fixnum x y))
      (multiple-value-bind (r g b)
          (funcall fn x y)
        (rgb->alpha r g b)))))
(defmethod image-rgb-blend-fn ((image rgb-image-mixin) &key (dx 0) (dy 0))
  (let ((sfn (image-rgb-set-fn image :dx dx :dy dy))
        (gfn (image-rgb-get-fn image :dx dx :dy dy)))
    (declare (type image-rgb-get-fn gfn)
             (type image-rgb-set-fn sfn))
    (lambda (x y red green blue alpha)
      (declare (type fixnum x y red green blue alpha))
      (multiple-value-bind (r g b) (funcall gfn x y)
        (multiple-value-bind (nr ng nb)
            (octet-rgb-blend-function red green blue alpha r g b)
        (funcall sfn x y nr ng nb))))))
(defmethod image-rgb-xor-blend-fn ((image rgb-image-mixin) &key (dx 0) (dy 0))
  (let ((sfn (image-rgb-set-fn image :dx dx :dy dy))
        (gfn (image-rgb-get-fn image :dx dx :dy dy)))
    (declare (type image-rgb-get-fn gfn)
             (type image-rgb-set-fn sfn))
    (lambda (x y red green blue alpha)
      (declare (type fixnum x y red green blue alpha))
      (multiple-value-bind (r g b) (funcall gfn x y)
        (multiple-value-bind (nr ng nb)
            (octet-rgb-blend-function
            (color-octet-xor r red)
            (color-octet-xor g green)
            (color-octet-xor b blue)
            alpha r g b)
          (funcall sfn x y nr ng nb))))))

;;; rgba
(defmethod image-alpha-get-fn ((image rgba-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-rgba-get-fn image :dx dx :dy dy :region region)))
    (declare (type image-rgba-get-fn fn))
    (lambda (x y)
      (declare (type fixnum x y))
      (multiple-value-bind (r g b a)
          (funcall fn x y)
        (rgba->alpha r g b a)))))

(defmethod image-gray-alpha-get-fn ((image rgba-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-rgba-get-fn image :dx dx :dy dy :region region)))
    (declare (type image-rgba-get-fn fn))
    (lambda (x y)
      (declare (type fixnum x y))
      (multiple-value-bind (r g b a)
          (funcall fn x y)
        (rgba->gray-alpha r g b a)))))

(defmethod image-rgba-blend-fn ((image rgba-image-mixin) &key (dx 0) (dy 0))
  (let ((sfn (image-rgba-set-fn image :dx dx :dy dy))
        (gfn (image-rgba-get-fn image :dx dx :dy dy)))
    (declare (type image-rgba-get-fn gfn)
             (type image-rgba-set-fn sfn))
    (lambda (x y red green blue alpha)
      (declare (type fixnum x y red green blue alpha))
      (multiple-value-bind (r g b a) (funcall gfn x y)
        (multiple-value-bind (nr ng nb na)
            (octet-rgba-blend-function red green blue alpha r g b a)
          (funcall sfn x y nr ng nb na))))))

(defmethod image-alpha-set-fn ((image rgba-image-mixin) &key (dx 0) (dy 0))
  (let ((sfn (image-rgba-set-fn image :dx dx :dy dy))
        (gfn (image-rgba-get-fn image :dx dx :dy dy)))
    (declare (type image-rgba-get-fn gfn)
             (type image-rgba-set-fn sfn))
    (lambda (x y alpha)
      (declare (type fixnum x y alpha))
      (multiple-value-bind (r g b a) (funcall gfn x y)
        (declare (ignore a))
        (funcall sfn x y r g b alpha)))))
