(in-package :clim-internals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; core protocol

(defgeneric sheet-enabled-p (sheet))
(defgeneric (setf sheet-enabled-p) (enabled-p sheet))
(defgeneric sheet-enabled-children (sheet))
(defgeneric sheet-viewable-p (sheet))
(defgeneric sheet-pointer-cursor (sheet))
(defgeneric (setf sheet-pointer-cursor) (cursor sheet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; relationship protocol

(defgeneric sheet-parent (sheet))
(defgeneric sheet-children (sheet))
(defgeneric sheet-siblings (sheet))
(defgeneric sheet-ancestor-p (sheet putative-ancestor))
(defgeneric map-over-sheets (function sheet))
(defgeneric sheet-adopt-child (sheet child))
(defgeneric sheet-disown-child (sheet child &key errorp))
(defgeneric raise-sheet (sheet))
(defgeneric bury-sheet (sheet))
(defgeneric reorder-sheets (sheet new-ordering))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; condition protocol

(define-condition sheet-is-not-child (error) ())
(define-condition sheet-ordering-underspecified (error) ())
(define-condition sheet-already-has-parent (error) ())
(define-condition sheet-is-ancestor (error) ())
(define-condition sheet-is-not-ancestor (error) ())
(define-condition sheet-supports-only-one-child (error)
  ((sheet :initarg :sheet)))

(defmethod print-object ((object sheet-supports-only-one-child) stream)
  (format stream "~A~%single-child-composite-pane is allowed to have only one child."
          (slot-value object 'sheet)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; geometry protocol

(defgeneric sheet-transformation (sheet))
(defgeneric (setf sheet-transformation) (transformation sheet))
(defgeneric sheet-region (sheet))
(defgeneric (setf sheet-region) (region sheet))
(defgeneric move-sheet (sheet x y))
(defgeneric resize-sheet (sheet width height))
(defgeneric move-and-resize-sheet (sheet x y width height))
(defgeneric map-over-sheets-containing-position (function sheet x y))
(defgeneric map-over-sheets-overlapping-region (function sheet region))
(defgeneric child-containing-position (sheet x y))
(defgeneric children-overlapping-region (sheet region))
(defgeneric children-overlapping-rectangle* (sheet x1 y1 x2 y2))
(defgeneric sheet-delta-transformation (sheet ancestor))
(defgeneric map-sheet-position-to-parent (sheet x y))
(defgeneric map-sheet-position-to-child (sheet x y))
(defgeneric map-sheet-rectangle*-to-parent (sheet x1 y1 x2 y2))
(defgeneric map-sheet-rectangle*-to-child (sheet x1 y1 x2 y2))
(defgeneric sheet-allocated-region (sheet child))

(defgeneric sheet-native-region (sheet))
(defgeneric sheet-native-transformation (sheet))
(defgeneric sheet-device-transformation (sheet))
(defgeneric sheet-device-region (sheet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; geometry cache protocol

(defgeneric invalidate-cached-regions (sheet))
(defgeneric invalidate-cached-transformations (sheet))
(defgeneric invalidate-cached-device-transformations (sheet))
(defgeneric invalidate-cached-device-regions (sheet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mirror protocol

(defparameter *configuration-event-p* nil
  "Flag used to inhibit setting mirror region and transformation to prevent
infinite recursion on (setf sheet-*).")

(defgeneric sheet-direct-mirror (sheet))
(defgeneric sheet-mirrored-ancestor (sheet))
(defgeneric sheet-mirror (sheet))
(defgeneric request-move-sheet (sheet x y))
(defgeneric request-resize-sheet (sheet width height))
(defgeneric request-change-sheet-region (sheet region))
(defgeneric request-change-sheet-transformation (sheet transformation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; graft protocol

(defgeneric graft (sheet))
(defgeneric sheet-grafted-p (sheet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; notification protocol

(defgeneric note-sheet-enabled (sheet))
(defgeneric note-sheet-disabled (sheet))
(defgeneric note-sheet-adopted (sheet))
(defgeneric note-sheet-disowned (sheet))
(defgeneric note-children-order-changed (sheet old-order))
(defgeneric note-sheet-grafted (sheet))
(defgeneric note-sheet-degrafted (sheet))
(defgeneric note-sheet-region-changed (sheet old-region))
(defgeneric note-sheet-transformation-changed (sheet old-transformation))

(defgeneric note-successor-enabled (sheet successor))
(defgeneric note-successor-disabled (sheet successor))
(defgeneric note-successor-adopted (sheet successor))
(defgeneric note-successor-disowned (sheet successor))
(defgeneric note-successor-order-changed (sheet successor old-order))
(defgeneric note-successor-region-changed (sheet successsor old-region))
(defgeneric note-successor-transformation-changed (sheet successor old-transformation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; protocol mixins

(defclass sheet-mixin ()
  ((enabled-p :type boolean
	      :initarg :enabled-p
              :initform t
              :accessor sheet-enabled-p)
   (region :type region
	   :initarg :region
	   :accessor sheet-region)
   (pointer-cursor :accessor sheet-pointer-cursor
                   :initarg  :pointer-cursor
                   :initform :default)))

;; relashionship

(defclass sheet-parent-mixin ()
  ((parent :initform nil :accessor sheet-parent)))

(defclass sheet-leaf-mixin () ())

(defclass sheet-single-child-mixin ()
  ((child :initform nil :accessor sheet-child)))

(defclass sheet-multiple-child-mixin ()
  ((children :initform nil :accessor sheet-children)))

;; geometry

(defclass sheet-identity-transformation-mixin ()
  ())

(defclass sheet-transformation-mixin ()
  ((transformation :initform +identity-transformation+
		   :initarg :transformation
		   :accessor sheet-transformation)))

(defclass sheet-translation-transformation-mixin (sheet-transformation-mixin)
  ())

(defclass sheet-y-inverting-transformation-mixin (sheet-transformation-mixin)
  ()
  (:default-initargs :transformation (make-transformation 1 0 0 -1 0 0)))

;; cached

(defclass sheet-cached-mixin ()
  ((cached-native-region :type (or null region)
		         :initform nil)
   (cached-native-transformation :type (or null transformation)
		                 :initform nil)
   (cached-device-region :type (or null region)
		         :initform nil)
   (cached-device-transformation :type (or null transformation)
		                 :initform nil)))
;; dirty region

(defclass sheet-dirty-region-mixin ()
  ((dirty-region :initform +nowhere+
                 :accessor sheet-dirty-region)))

;; mirrored

(defclass mirrored-sheet-mixin (sheet-dirty-region-mixin)
  ((port :initform nil :initarg :port :accessor port)
   (mirror :initform nil :accessor sheet-direct-mirror)
   (mirror-transformation
    :initform +identity-transformation+
    :accessor %sheet-mirror-transformation)
   (mirror-region
    :initform nil
    :accessor %sheet-mirror-region)))

;; graft

(defclass sheet-graft-mixin ()
  ())




