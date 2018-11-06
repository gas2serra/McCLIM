(in-package :clim-internals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; core protocol

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mirrored sheet protocol

(defgeneric port-realize-mirror (port sheet))
(defgeneric port-destroy-mirror (port sheet))
(defgeneric port-enable-mirror  (port sheet))
(defgeneric port-disable-mirror (port sheet))
(defgeneric port-set-mirror-region (port sheet region))
(defgeneric port-set-mirror-transformation (port sheet transformation))
(defgeneric port-set-sheet-pointer-cursor (port sheet cursor))
(defgeneric port-lookup-sheet (port mirror))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ??

(defgeneric mirror-id (mirror))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; protocol mixins

(defclass port-mixin ()
  ((mirror-id->sheet :initform (make-hash-table :test #'eq))))

#|

(defmethod port-lookup-current-pointer-cursor ((port standard-port) sheet)
  (gethash sheet (slot-value port 'mirrored-sheet->current-pointer-cursor)))

(defmethod port-set-sheet-pointer-cursor :before ((port standard-port)
                                                  sheet cursor)
  (setf (gethash sheet (slot-value port 'mirrored-sheet->current-pointer-cursor)) cursor))
((mirrored-sheet->current-pointer-cursor :initform (make-hash-table :test #'eq))
|#
