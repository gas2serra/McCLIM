(in-package :clim-internals)

(defclass space-requirement () ())

;;;;
;;;; standard-space-requirement
;;;;

(defclass standard-space-requirement (space-requirement)
  ((width      :initform 1
	       :initarg :width
	       :reader space-requirement-width)
   (max-width  :initform 1
	       :initarg :max-width
	       :reader space-requirement-max-width)
   (min-width  :initform 1
	       :initarg :min-width
	       :reader space-requirement-min-width)
   (height     :initform 1
	       :initarg :height
	       :reader space-requirement-height)
   (max-height :initform 1
	       :initarg :max-height
	       :reader space-requirement-max-height)
   (min-height :initform 1
	       :initarg :min-height
	       :reader space-requirement-min-height) ) )

(defconstant +fill+ (expt 10 (floor (log most-positive-fixnum 10))))

(declfun make-space-requirement (&key (min-width 0) (min-height 0)
                                 (width min-width) (height min-height)
                                 (max-width +fill+) (max-height +fill+)))

(declfun space-requirement-combine* (function sr1 &key (width 0) (min-width 0) (max-width 0)
                                              (height 0) (min-height 0) (max-height 0)))
  
(declfun space-requirement-combine (function sr1 sr2))
(declfun space-requirement+ (sr1 sr2))

(declfun space-requirement+* (space-req &key (width 0) (min-width 0) (max-width 0)
                                        (height 0) (min-height 0) (max-height 0)))

;; Macros for quick access to space-requirement slots.
(defmacro sr-width (pane)
  `(space-requirement-width (pane-space-requirement ,pane)))
(defmacro sr-height (pane)
  `(space-requirement-height (pane-space-requirement ,pane)))
(defmacro sr-max-width (pane)
  `(space-requirement-max-width (pane-space-requirement ,pane)))
(defmacro sr-max-height (pane)
  `(space-requirement-max-height (pane-space-requirement ,pane)))
(defmacro sr-min-width (pane)
  `(space-requirement-min-width (pane-space-requirement ,pane)))
(defmacro sr-min-height (pane)
  `(space-requirement-min-height (pane-space-requirement ,pane)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; User Space Requirements
;;; Space Requirements

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun spacing-value-p (x)
    (or (and (realp x) (>= x 0))
        (and (consp x)
             (realp (car x))
             (consp (cdr x))
             (member (cadr x) '(:point :pixel :mm :character :line))
             (null (cddr x)))
        (eq x :compute))))

(deftype spacing-value ()
  ;; just for documentation
  `(satisfies spacing-value-p))

(defclass space-requirement-options-mixin ()
  ((user-width
    :initarg  :width
    :initform nil
    :reader   pane-user-width
    :type     (or null spacing-value))
   (user-min-width
    :initarg :min-width
    :initform nil
    :reader   pane-user-min-width
    :type     (or null spacing-value))
   (user-max-width
    :initarg :max-width
    :initform nil
    :reader   pane-user-max-width
    :type     (or null spacing-value))
   (user-height
    :initarg :height
    :initform nil
    :reader   pane-user-height
    :type     (or null spacing-value))
   (user-min-height
    :initarg :min-height
    :initform nil
    :reader   pane-user-min-height
    :type     (or null spacing-value))
   (user-max-height
    :initarg :max-height
    :initform nil
    :reader   pane-user-max-height
    :type     (or null spacing-value))
   (x-spacing
    :initarg :x-spacing
    :initform 0
    :reader   pane-x-spacing
    :type     (or null spacing-value))
   (y-spacing
    :initarg :y-spacing
    :initform 0
    :reader   pane-y-spacing
    :type     (or null spacing-value)))
  (:documentation
   "Mixin class for panes which offer the standard user space requirements options."))

(defclass standard-space-requirement-options-mixin (space-requirement-options-mixin)
  ())

(defgeneric spacing-value-to-device-units (pane x))
(defgeneric change-space-requirements (pane &key width min-width max-width
                                              height min-height max-height
                                              x-spacing y-spacing
                                              &allow-other-keys))

(defgeneric merge-user-specified-options (pane sr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Layout


(defclass layout-protocol-mixin ()
  ((space-requirement :accessor pane-space-requirement
                      :initform nil
                      :documentation "The cache of the space requirements of the pane. NIL means: need to recompute.")
   (current-width     :accessor pane-current-width
                      :initform nil)
   (current-height    :accessor pane-current-height
                      :initform nil) ))

(defgeneric allocate-space (pane width height))
(defgeneric compose-space (pane &key width height))


(defclass top-level-sheet-pane-mixin ()
  ())
