(in-package :clim-clx-fb)

(defclass clx-fb-port (fb-port
		       clim-xcommon:keysym-port-mixin
		       clim-clx::clx-basic-port)
  ())

(defun parse-clx-fb-server-path (path)
  (let ((server-path (clim-clx::parse-clx-server-path path)))
    (pop path)
    (cons :clx-fb (cdr server-path))))

(setf (get :clx-fb :port-type) 'clx-fb-port)
(setf (get :clx-fb :server-path-parser) 'parse-clx-fb-server-path)

(defmethod initialize-instance :after ((port clx-fb-port) &rest args)
  (declare (ignore args))
  (push (make-instance 'clx-fb-frame-manager :port port)
	(slot-value port 'frame-managers))
  (setf (slot-value port 'pointer)
	(make-instance 'clim-clx::clx-basic-pointer :port port))
  (initialize-clx port)
  (initialize-clx-framebuffer port)
  (clim-extensions:port-all-font-families port))


(defun initialize-clx-framebuffer (port)
  (clim-sys:make-process (lambda ()
                           (loop
                              (handler-case
                                  (fb-port-flush-mirrors port)
                                (condition (condition)
                                  (format *debug-io* "~A~%" condition)))
                              (xlib:display-force-output (clx-port-display port))
                              (sleep 0.01)))
                         :name (format nil "~S's event process." port)))

(defparameter *event-mask* '(:exposure 
			     :key-press :key-release
			     :button-press :button-release
			     :owner-grab-button
			     :enter-window :leave-window
			     :structure-notify
			     :pointer-motion :button-motion))

(defmethod fb-port-realize-real-mirror ((port clx-fb-port) (sheet top-level-sheet-pane))
  (let ((q (compose-space sheet)))
    (let ((frame (pane-frame sheet))
          (window (realize-mirror-aux port sheet
				      :event-mask *event-mask*
                                      :map nil
                                      :width (clim-clx::round-coordinate (space-requirement-width q))
                                      :height (clim-clx::round-coordinate (space-requirement-height q)))))
      (setf (xlib:wm-hints window) (xlib:make-wm-hints :input :on))
      (setf (xlib:wm-name window) (frame-pretty-name frame))
      (setf (xlib:wm-icon-name window) (frame-pretty-name frame))
      (xlib:set-wm-class
       window
       (string-downcase (frame-name frame))
       (string-capitalize (string-downcase (frame-name frame))))
      (setf (xlib:wm-protocols window) `(:wm_delete_window))
      (xlib:change-property window
                            :WM_CLIENT_LEADER (list (xlib:window-id window))
                            :WINDOW 32)
      window)))

(defmethod fb-port-realize-real-mirror ((port clx-fb-port) (sheet unmanaged-top-level-sheet-pane))
 (realize-mirror-aux port sheet
		      :event-mask *event-mask*
		      :override-redirect :on
		      :map nil))
  
(defmethod fb-port-realize-mirror ((port clx-fb-port) real-mirror)
  (make-instance 'clx-fb-mirror :real-mirror real-mirror))

(defmethod fb-port-realize-pixmap ((port clx-fb-port) sheet width height)
    (make-instance 'clx-fb-pixmap
                   :sheet sheet
                   :width width
                   :height height
                   :port port))

(defmethod make-graft ((port clx-fb-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'clx-graft
		 :port port :mirror (clx-port-window port)
		 :orientation orientation :units units)))
    (setf (sheet-region graft) (make-bounding-rectangle 0 0 (xlib:screen-width (clx-port-screen port)) (xlib:screen-height (clx-port-screen port))))
    (push graft (port-grafts port))
    graft))

(defmethod graft ((port clx-fb-port))
  (first (port-grafts port)))


(defmethod port-force-output ((port clx-fb-port))
  (maphash #'(lambda (key val)
               (when (typep key 'clx-fb-mirrored-sheet-mixin)
                 (mcclim-render::%mirror-force-output (sheet-mirror key))))
           (slot-value port 'climi::sheet->mirror))
  (xlib:display-force-output (clx-port-display port)))

(defmethod get-next-event ((port clx-fb-port) &key wait-function (timeout nil))
  (declare (ignore wait-function))
  (let* ((clim-clx::*clx-port* port)
         (display (clx-port-display port)))
    (unless (xlib:event-listen display)
      (xlib:display-force-output (clx-port-display port)))
    (let ((event (xlib:process-event (clx-port-display port)
				     :handler #'clim-clx::event-handler :discard-p t)))
      (if event
	  event
	  :timeout))))

;;; Pixmap


(defun realize-mirror-aux (port sheet
				&key (width 100) (height 100) (x 0) (y 0)
				(border-width 0) (border 0)
				(override-redirect :off)
				(map nil)
				(backing-store :not-useful)
                                (save-under :off)
				(event-mask `(:exposure 
					      :key-press :key-release
					      :button-press :button-release
					      :enter-window :leave-window
					      :structure-notify
					      :pointer-motion
					      :button-motion)))
  (declare (ignore border-width))
  (when (null (port-lookup-mirror port sheet))
    ;;(update-mirror-geometry sheet (%%sheet-native-transformation sheet))
    (let* ((desired-color (typecase sheet
                            (permanent-medium-sheet-output-mixin ;; sheet-with-medium-mixin
                              (medium-background sheet))
                            (basic-pane ; CHECKME [is this sensible?] seems to be
                              (let ((background (pane-background sheet)))
                                (if (typep background 'color)
                                    background
                                    +white+)))
                            (t
                              +white+)))
           (color (multiple-value-bind (r g b)
                      (color-rgb desired-color)
                    (xlib:make-color :red r :green g :blue b)))
	   (screen (clx-port-screen port))
           (pixel (xlib:alloc-color (xlib:screen-default-colormap screen) color))
	   (mirror-region (%sheet-mirror-region sheet))
	   (mirror-transformation (%sheet-mirror-transformation sheet))
           (window (xlib:create-window
                    :parent (sheet-xmirror (sheet-parent sheet))
                    :width (if mirror-region
                               (round-coordinate (bounding-rectangle-width mirror-region))
                               width)
                    :height (if mirror-region
				(round-coordinate (bounding-rectangle-height mirror-region))
				height)
                    :x (if mirror-transformation
			   (round-coordinate (nth-value 0 (transform-position
							   mirror-transformation
							   0 0)))
			   x)
                    :y (if mirror-transformation
                           (round-coordinate (nth-value 1 (transform-position
                                                           mirror-transformation
                                                           0 0)))
                           y)
                    :border-width 0 ;;border-width
                    :border border
                    :override-redirect override-redirect
                    :backing-store backing-store
                    :save-under save-under
                    :gravity :north-west
                    ;; Evil Hack -- but helps enormously (Has anybody
                    ;; a good idea how to sneak the concept of
                    ;; bit-gravity into CLIM)? --GB
                    :bit-gravity (if (typep sheet 'climi::extended-output-stream)
                                     :north-west
                                     :forget)
                    :background pixel
                    :event-mask (apply #'xlib:make-event-mask
                                       event-mask))))
      (port-register-mirror (port sheet) sheet window)
      (when map
        (xlib:map-window window))
      window)))
