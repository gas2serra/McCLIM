(in-package :clim-fb)

(defclass fb-port (standard-port
                   standard-handled-event-port-mixin
                   render-port-mixin)
  ())

(defmethod make-medium ((port fb-port) sheet)
  (make-instance 'fb-medium :sheet sheet))

(defgeneric fb-port-flush-mirrors (port)
  (:method ((port fb-port))
    (maphash #'(lambda (key val)
                 (when (typep key 'fb-mirrored-sheet-mixin)
                   (fb-mirror-flush (sheet-mirror key))))
             (slot-value port 'climi::sheet->mirror))))

(defgeneric fb-port-realize-real-mirror (port sheet))
(defgeneric fb-port-realize-mirror (port real-mirror))
(defgeneric fb-port-realize-pixmap (port sheet width height))

(defmethod realize-mirror ((port fb-port) (sheet mirrored-sheet-mixin))
  (let ((real-mirror (fb-port-realize-real-mirror port sheet)))
    (port-register-mirror (port sheet) sheet
                          (fb-port-realize-mirror port real-mirror)))
  (port-lookup-mirror port sheet))

(defmethod realize-mirror ((port fb-port) (pixmap fb-pixmap))
  (call-next-method))

(defmethod destroy-mirror ((port fb-port) (pixmap fb-pixmap))
  (call-next-method))

(defmethod realize-mirror ((port fb-port) (pixmap fb-pixmap))
  (setf (sheet-parent pixmap) (graft port))
  (let ((mirror (make-instance 'opticl-rgb-image-mirror-mixin)))
    (port-register-mirror port pixmap mirror)
    (%make-image mirror pixmap)))

(defmethod port-allocate-pixmap ((port fb-port) sheet width height)
  (let ((pixmap (fb-port-realize-pixmap port sheet width height)))
    (when (sheet-grafted-p sheet)
      (realize-mirror port pixmap))
    pixmap))

(defmethod port-deallocate-pixmap ((port fb-port) pixmap)
  (when (port-lookup-mirror port pixmap)
    (destroy-mirror port pixmap)))
