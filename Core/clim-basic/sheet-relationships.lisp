(in-package :clim-internals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; relationship protocol

(defgeneric sheet-parent (sheet))
(defgeneric sheet-children (sheet))
(defgeneric sheet-adopt-child (sheet child))
(defgeneric sheet-disown-child (sheet child &key errorp))
(defgeneric sheet-siblings (sheet))
(defgeneric sheet-ancestor-p (sheet putative-ancestor))
(defgeneric raise-sheet (sheet))
(defgeneric bury-sheet (sheet))
(defgeneric reorder-sheets (sheet new-ordering))
(defgeneric map-over-sheets (function sheet))
(defgeneric sheet-enabled-children (sheet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; notification protocol

(defgeneric note-sheet-adopted (sheet))
(defgeneric note-sheet-disowned (sheet))
(defgeneric note-sheet-order-changed (sheet old-order))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; protocol mixins

(defclass sheet-parent-mixin ()
  ((parent :initform nil :accessor sheet-parent)))

(defclass sheet-leaf-mixin () ())

(defclass sheet-single-child-mixin ()
  ((child :initform nil :accessor sheet-child)))

(defclass sheet-multiple-child-mixin ()
  ((children :initform nil :accessor sheet-children)))

