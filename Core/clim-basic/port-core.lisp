(in-package :clim-internals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mirrored sheet protocol

(defmethod port-lookup-sheet ((port port-mixin) mirror)
  (gethash (mirror-id mirror) (slot-value port 'mirror-id->sheet)))

(defmethod port-realize-mirror ((port port-mixin) (sheet mirrored-sheet-mixin))
  (error "Don't know how to realize the mirror of a generic mirrored sheet"))

(defmethod port-realize-mirror :after ((port port-mixin) (sheet mirrored-sheet-mixin))
  (assert (sheet-mirror sheet))
  (setf (gethash (mirror-id (sheet-direct-mirror sheet))
                 (slot-value port 'mirror-id->sheet))
        sheet))

(defmethod port-destroy-mirror ((port port-mixin) (sheet mirrored-sheet-mixin))
  (error "Don't know how to destroy the mirror of a generic mirrored sheet"))

(defmethod port-destroy-mirror :before ((port port-mixin) (sheet mirrored-sheet-mixin))
  (assert (sheet-mirror sheet))
  (remhash (mirror-id (sheet-direct-mirror sheet))
           (slot-value port 'mirror-id->sheet)))

(defmethod port-destroy-mirror :after ((port port-mixin) (sheet mirrored-sheet-mixin))
  (assert (sheet-mirror sheet))
  (setf (sheet-direct-mirror sheet) nil))

(defmethod port-enable-mirror ((port port-mixin) (sheet mirrored-sheet-mixin))
  (error "Don't know how to enable the mirror of a generic mirrored sheet"))

(defmethod port-disable-mirror ((port port-mixin) (sheet mirrored-sheet-mixin))
  (error "Don't know how to disable the mirror of a generic mirrored sheet"))

(defmethod port-set-mirror-region ((port port-mixin) (sheet mirrored-sheet-mixin) region)
  (declare (ignore sheet region))
  (error "Don't know how to set the mirror region of a generic mirrored sheet"))

(defmethod port-set-mirror-transformation ((port port-mixin) (sheet sheet-mixin) transformation)
  (declare (ignore sheet transformation))
  (error "Don't know how to set the mirror transformation of a generic mirrored sheet"))

(defmethod port-set-sheet-pointer-cursor ((port port-mixin) sheet cursor)
  (declare (ignore sheet cursor))
  (warn "Port ~A has not implemented sheet pointer cursors." port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ??

(defmethod mirror-id ((mirror t))
  mirror)
