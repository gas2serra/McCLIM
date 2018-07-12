(in-package :mcclim-render-internals)

;;;
;;; image manipulation functions
;;;
(defgeneric image-pixels-type (image-class))

;; rgba
(deftype image-rgba-get-fn () '(function (fixnum fixnum) (values octet octet octet octet)))
(deftype image-gray-alpha-get-fn () '(function (fixnum fixnum) (values octet octet)))
(deftype image-rgba-set-fn () '(function (fixnum fixnum octet octet octet octet)))
(deftype image-rgba-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
;; rgb
(deftype image-rgb-get-fn () '(function (fixnum fixnum) (values octet octet octet)))
(deftype image-rgb-set-fn () '(function (fixnum fixnum octet octet octet)))
(deftype image-rgb-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
(deftype image-rgb-xor-blend-fn () '(function (fixnum fixnum octet octet octet octet)))
;; gray
(deftype image-gray-get-fn () '(function (fixnum fixnum) octet))
(deftype image-gray-set-fn () '(function (fixnum fixnum octet)))
(deftype image-gray-blend-fn () '(function (fixnum fixnum octet octet)))
;; alpha
(deftype image-alpha-get-fn () '(function (fixnum fixnum) octet))
(deftype image-alpha-set-fn () '(function (fixnum fixnum octet )))
(deftype image-alpha-blend-fn () '(function (fixnum fixnum octet octet)))

;; rgba
(defgeneric image-rgba-get-fn (image &key dx dy region))
(defgeneric image-gray-alpha-get-fn (image &key dx dy region))
(defgeneric image-rgba-set-fn (image &key dx dy))
(defgeneric image-rgba-blend-fn (image &key dx dy))
;; rgb
(defgeneric image-rgb-get-fn (image &key dx dy region))
(defgeneric image-rgb-set-fn (image &key dx dy))
(defgeneric image-rgb-blend-fn (image &key dx dy))
(defgeneric image-rgb-xor-blend-fn (image &key dx dy))
;; gray
(defgeneric image-gray-get-fn (image &key dx dy region))
(defgeneric image-gray-set-fn (image &key dx dy))
(defgeneric image-gray-blend-fn (image &key dx dy))
;; alpha
(defgeneric image-alpha-get-fn (image &key dx dy region))
(defgeneric image-alpha-set-fn (image &key dx dy))
(defgeneric image-alpha-blend-fn (image &key dx dy))

;;;
;;; defaults
;;;

;; image
(defmethod map-rgb-color ((image image-mixin) fn &key (x 0) (y 0)
                                                   (width (image-width image))
                                                   (height (image-height image)))
  (let ((gfn (image-rgb-get-fn image)))
    (declare (type fixnum x y width height))
    (loop for i from x to (+ x width -1) do
         (loop for j from y to (+ y height -1) do
              (multiple-value-bind (red green blue)
                  (funcall gfn i j)
                (funcall fn i j red green blue))))))

;; gray
(defmethod image-alpha-get-fn ((image gray-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-gray-get-fn image :dx dx :dy dy :region region)))
    (lambda (x y)
      (declare (type fixnum x y))
      (gray->alpha (funcall fn x y)))))

(defmethod image-rgba-get-fn ((image gray-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-gray-get-fn image :dx dx :dy dy :region region)))
    (lambda (x y)
      (declare (type fixnum x y))
      (gray->rgba (funcall fn x y)))))

(defmethod image-rgb-get-fn ((image gray-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-gray-get-fn image :dx dx :dy dy :region region)))
    (lambda (x y)
      (declare (type fixnum x y))
      (gray->rgb (funcall fn x y)))))

(defmethod image-alpha-set-fn ((image gray-image-mixin) &key (dx 0) (dy 0))
  (let ((fn (image-gray-set-fn image :dx dx :dy dy)))
    (lambda (x y alpha)
      (declare (type fixnum x y alpha))
      (funcall fn x y (alpha->gray alpha)))))

(defmethod image-gray-blend-fn ((image gray-image-mixin) &key (dx 0) (dy 0))
  (let ((sfn (image-gray-set-fn image :dx dx :dy dy))
        (gfn (image-gray-get-fn image :dx dx :dy dy)))
    (lambda (x y gray alpha)
      (declare (type fixnum x y gray alpha))
      (let ((g (funcall gfn x y)))
        (funcall sfn x y (octet-gray-blend-function gray alpha g))))))

;; rgb
(defmethod image-gray-get-fn ((image  rgb-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-rgb-get-fn image :dx dx :dy dy :region region)))
    (lambda (x y)
      (declare (type fixnum x y))
      (multiple-value-bind (r g b)
          (funcall fn x y)
        (rgb->gray r g b)))))

(defmethod image-alpha-get-fn ((image rgb-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-rgb-get-fn image :dx dx :dy dy :region region)))
    (lambda (x y)
      (declare (type fixnum x y))
      (multiple-value-bind (r g b)
          (funcall fn x y)
        (rgb->alpha r g b)))))

(defmethod image-rgba-get-fn ((image rgb-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-rgb-get-fn image :dx dx :dy dy :region region)))
    (lambda (x y)
      (declare (type fixnum x y))
      (multiple-value-bind (r g b)
          (funcall fn x y)
        (rgb->rgba r g b)))))

(defmethod image-rgb-blend-fn ((image rgb-image-mixin) &key (dx 0) (dy 0))
  (let ((sfn (image-rgb-set-fn image :dx dx :dy dy))
        (gfn (image-rgb-get-fn image :dx dx :dy dy)))
    (lambda (x y red green blue alpha)
      (declare (type fixnum x y red green blue alpha))
      (multiple-value-bind (r g b) (funcall gfn x y)
        (multiple-value-bind (nr ng nb)
            (octet-rgb-blend-function red green blue alpha r g b)
        (funcall sfn x y nr ng nb))))))

(defmethod image-rgb-xor-blend-fn ((image rgb-image-mixin) &key (dx 0) (dy 0))
  (let ((sfn (image-rgb-set-fn image :dx dx :dy dy))
        (gfn (image-rgb-get-fn image :dx dx :dy dy)))
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
(defmethod image-gray-get-fn ((image rgba-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-rgba-get-fn image :dx dx :dy dy :region region)))
    (lambda (x y)
      (declare (type fixnum x y))
      (multiple-value-bind (r g b a)
          (funcall fn x y)
        (rgba->gray r g b a)))))

(defmethod image-alpha-get-fn ((image rgba-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-rgba-get-fn image :dx dx :dy dy :region region)))
    (lambda (x y)
      (declare (type fixnum x y))
      (multiple-value-bind (r g b a)
          (funcall fn x y)
        (rgba->alpha r g b a)))))

(defmethod image-rgb-get-fn ((image rgba-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-rgba-get-fn image :dx dx :dy dy :region region)))
    (lambda (x y)
      (declare (type fixnum x y))
      (multiple-value-bind (r g b a)
          (funcall fn x y)
        (rgba->rgb r g b a)))))

(defmethod image-gray-alpha-get-fn ((image rgba-image-mixin) &key (dx 0) (dy 0) (region nil))
  (let ((fn (image-rgba-get-fn image :dx dx :dy dy :region region)))
    (lambda (x y)
      (declare (type fixnum x y))
      (multiple-value-bind (r g b a)
          (funcall fn x y)
        (rgba->gray-alpha r g b a)))))

(defmethod image-rgba-blend-fn ((image rgba-image-mixin) &key (dx 0) (dy 0))
  (let ((sfn (image-rgba-set-fn image :dx dx :dy dy))
        (gfn (image-rgba-get-fn image :dx dx :dy dy)))
    (lambda (x y red green blue alpha)
      (declare (type fixnum x y red green blue alpha))
      (multiple-value-bind (r g b a) (funcall gfn x y)
        (multiple-value-bind (nr ng nb na)
            (octet-rgba-blend-function red green blue alpha r g b a)
          (funcall sfn x y nr ng nb na))))))

(defmethod image-alpha-set-fn ((image rgba-image-mixin) &key (dx 0) (dy 0))
  (let ((sfn (image-rgba-set-fn image :dx dx :dy dy))
        (gfn (image-rgba-get-fn image :dx dx :dy dy)))
    (lambda (x y alpha)
      (declare (type fixnum x y alpha))
      (multiple-value-bind (r g b a) (funcall gfn x y)
        (funcall sfn x y r g b alpha)))))
