(in-package :clim-internals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; core protocol

(defgeneric sheet-enabled-p (sheet))
(defgeneric (setf sheet-enabled-p) (enabled-p sheet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; notification protocol

(defgeneric note-sheet-enabled (sheet))
(defgeneric note-sheet-disabled (sheet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; protocol mixins

(defclass sheet-mixin ()
  ((enabled-p :type boolean
	      :initarg :enabled-p
              :initform t
              :accessor sheet-enabled-p)))

(defmethod note-sheet-enabled ((sheet sheet-mixin))
  (declare (ignorable sheet))
  nil)

(defmethod note-sheet-disabled ((sheet sheet-mixin))
  (declare (ignorable sheet))
  nil)

(defmethod (setf sheet-enabled-p) :around (enabled-p (sheet sheet-mixin))
  (unless (eql enabled-p (sheet-enabled-p sheet))
    (call-next-method)
    (if enabled-p
        (note-sheet-enabled sheet)
        (note-sheet-disabled sheet))))
