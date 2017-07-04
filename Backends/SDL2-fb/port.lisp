
(in-package :clim-sdl2)


(declaim (inline round-coordinate))
(defun round-coordinate (x)
  "Function used for rounding coordinates."
  (floor (+ x .5)))

;;;
;;; sdl2 port
;;;

(defclass sdl2-port (fb-port)
  ((id)
   (sheet->window :initform (make-hash-table))
   (mirror->window :initform (make-hash-table))
   (window->sheet :initform (make-hash-table))
   (window->win :initform (make-hash-table))
   (pointer :reader port-pointer)))


(defgeneric initialize-sdl2 (port))

(defmethod initialize-instance :after ((port sdl2-port) &rest args)
  (declare (ignore args))
  (setf (slot-value port 'id) (gensym "SDL2-PORT-"))
  (push (make-instance 'sdl2-fb-frame-manager :port port)
	(slot-value port 'frame-managers))
  (setf (slot-value port 'pointer) (make-instance 'sdl2-pointer))
  (initialize-sdl2 port))

(defmethod print-object ((object sdl2-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S ~S" :id (slot-value object 'id))))

(defun parse-sdl2-server-path (path)
  path)

(setf (get :sdl2 :port-type) 'sdl2-port)
(setf (get :sdl2 :server-path-parser) 'parse-sdl2-server-path)

(defmethod initialize-sdl2 ((port sdl2-port))
  (let ((options (cdr (port-server-path port))))
    (start-sdl2-server port)
    (setf (port-event-process port) *sdl2-server*)
    (make-graft port)))

;;;
;;; sdl2 pointer 
;;;
(defclass sdl2-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

(defmethod fb-port-realize-real-mirror ((port sdl2-port)
                                        (sheet top-level-sheet-pane))
  (let* ((frame (pane-frame sheet))
	 (win (realize-mirror-aux port sheet :title (frame-pretty-name frame))))
    (with-slots (mirror->window) port
      (setf (gethash win mirror->window) win)
      (format t "REALIZE: ~A ~A~%" win win)
      (port-register-mirror (port sheet) sheet win)
      win)))

(defmethod fb-port-realize-real-mirror ((port sdl2-port)
                                        (sheet unmanaged-top-level-sheet-pane))
  (let* ((frame (pane-frame sheet))
         (win (realize-mirror-aux port sheet
                                  :flags '(:SKIP-TASKBAR :BORDERLESS)
                                  :title (frame-pretty-name frame))))
    (with-slots (mirror->window) port
      (setf (gethash win mirror->window) win)
      (format *debug-io* "REALIZE UN: ~A ~A~%" win win)
      (port-register-mirror (port sheet) sheet win)
      win)))

(defmethod fb-port-realize-mirror ((port sdl2-port)
                                   real-mirror)
  (let ((mirror (make-instance 'sdl2-fb-mirror :port port
                               :real-mirror real-mirror
                               :render (<+ `(sdl2:create-renderer (sdl2-ffi.functions::sdl-get-window-from-id ,real-mirror))))))
    (with-slots (mirror->window) port
      (setf (gethash mirror mirror->window) real-mirror))
    mirror))

(defparameter *flags* '(:resizable :HIDDEN))

(defun realize-mirror-aux (port sheet &key (flags *flags*) (width 200) (height 200) (x 0) (y 0) (title ""))
  (let ((mirror-region (%sheet-mirror-region sheet))
	(mirror-transformation (%sheet-mirror-transformation sheet)))
    (let ((window-flags (sdl2::mask-apply 'sdl2::sdl-window-flags flags))
	  (x (sdl2::windowpos-from-coord x))
	  (y (sdl2::windowpos-from-coord y)))
      (format *debug-io* "WINDOW FLAGS: ~A~%" window-flags)
    (let ((win (<+ 
		`(sdl2::sdl-create-window ,title
					  ,(if mirror-transformation
					       (round-coordinate (nth-value 0 (transform-position
									       mirror-transformation
									       0 0)))
					       x)
					  ,(if mirror-transformation
					       (round-coordinate (nth-value 1 (transform-position
									       mirror-transformation
									       0 0)))
					       y)
					  ,(if mirror-region
					       (round-coordinate (bounding-rectangle-width mirror-region))
					       width)
					  ,(if mirror-region
					       (round-coordinate (bounding-rectangle-height mirror-region))
					       height)
					  ',window-flags))))
      (let ((wid (<+ `(sdl2:get-window-id ,win))))
	(format *debug-io* "wid: ~A~%" wid)
	(with-slots (sheet->window window->sheet window->win) port
	  (setf (gethash wid window->sheet) sheet)
	  (setf (gethash wid window->win) win)
	  (setf (gethash sheet sheet->window) wid))
	wid)))))
  
(defmethod destroy-mirror ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (with-slots (sheet->window) port
    (let ((win (gethash sheet sheet->window)))
      (format *debug-io* "destroy~%")
      (when win
	(format *debug-io* "destroy~%")
	(<+ `(sdl2::sdl-destroy-window (sdl2-ffi.functions::sdl-get-window-from-id ,win)))))))


(defmethod get-next-event ((port sdl2-port) &key wait-function (timeout nil))
  (declare (ignore wait-function))
  ;;(format *debug-io* "IIII ~%")
  (setf sdl2::*event-loop* t)
  ;;(sdl2:in-main-thread ()
    
    (sdl2:with-sdl-event (event)
      (let ((r (sdl2:next-event event :wait-with-timeout (round (* 1000 timeout)))))
	(when (< r 0)
	  ;;(format *debug-io* "!!!! ~a~%" r)
	  (break))
	(when (= r 0)
	  ;;(format *debug-io* "!!!!TIMEOUT ~%")
	  ;;(break)
	  )
	(when (> r 0)
	  (let ((etype (sdl2:get-event-type event )))
	    ;;(format *debug-io* "Event: ~A ~A~%" etype r)
	    (case etype
	      (:lisp-message 
               ;;(format *debug-io* ">>> ~A~%" (sdl2::getmsg sdl2::*main-thread-channel*))
               (sdl2::get-and-handle-messages)
               nil)
	      (:windowevent
	       (let* ((e (sdl2::c-ref event sdl2-ffi:sdl-event :window :event))
		      (w (sdl2::c-ref event sdl2-ffi:sdl-event :window :window-id))
		      (time (sdl2::c-ref event sdl2-ffi:sdl-event :window :timestamp))
		      (data1 (sdl2::c-ref event sdl2-ffi:sdl-event :window :data1))	  
		      (data2 (sdl2::c-ref event sdl2-ffi:sdl-event :window :data2))		  
		      (sheet  (with-slots (window->sheet) port
				(gethash w window->sheet))))
		 ;;(format *debug-io* "E: ~A ~A ~A ~A~%" e w time sheet)
		 (when sheet
		   (let* ((mirror-region (%sheet-mirror-region sheet))
			  (mirror-transformation (%sheet-mirror-transformation sheet))
			  (x (round-coordinate (nth-value 0 (transform-position
							     mirror-transformation
							     0 0))))
			  (y (round-coordinate (nth-value 1 (transform-position
							     mirror-transformation
							     0 0))))
			  (width (if mirror-region (round-coordinate (bounding-rectangle-width mirror-region)) 0))
			  (height (if mirror-region (round-coordinate (bounding-rectangle-height mirror-region)) 0)))

		     (when sheet
		       (cond
			 ((= e sdl2-ffi::+sdl-windowevent-close+)
			  (make-instance 'window-manager-delete-event :sheet sheet :timestamp time))
			 ((= e sdl2-ffi::+sdl-windowevent-enter+)
			  (make-instance 'pointer-enter-event
					 :x 0 :y 0
					 :pointer 0 :button 0 
					 :graft-x 0 :graft-y 0
					 :modifier-state nil
					 :sheet sheet
					 :timestamp time))
			 ((= sdl2-ffi::+sdl-windowevent-exposed+ e)
			  (make-instance 'window-repaint-event
					 :timestamp time
					 :sheet sheet
					 :region (sheet-region sheet)))
			 ((= sdl2-ffi::+sdl-windowevent-focus-gained+ e)
			  nil)
			 ((= sdl2-ffi::+sdl-windowevent-focus-lost+ e)
			  nil)
			 ((= sdl2-ffi::+sdl-windowevent-hidden+ e)
			  nil)
			 ((= sdl2-ffi::+sdl-windowevent-leave+ e)
			  (make-instance 'pointer-exit-event
					 :x 0 :y 0
					 :pointer 0 :button 0 
					 :graft-x 0 :graft-y 0
					 :modifier-state nil
					 :x 0 :y 0
					 :sheet sheet
					 :timestamp time))
			 ((= sdl2-ffi::+sdl-windowevent-maximized+ e)
			  nil)
			 ((= sdl2-ffi::+sdl-windowevent-minimized+ e)
			  nil)
			 ((= sdl2-ffi::+sdl-windowevent-moved+ e)
			  ;;(make-instance 'window-configuration-event
			  ;;:sheet sheet
			  ;;             :x data1 :y data2 :width width :height height))
			  nil
			  )
			 ((= sdl2-ffi::+sdl-windowevent-none+ e)
			  nil)
			 ((= sdl2-ffi::+sdl-windowevent-resized+ e)
			  ;;(make-instance 'window-configuration-event
			  ;;             :sheet sheet
			  ;;             :x x :y y :width data1 :height data2))
			  nil
			  )
			 ((= sdl2-ffi::+sdl-windowevent-restored+ e)
			  nil)
			 ((= sdl2-ffi::+sdl-windowevent-shown+ e)
			  nil)
			 ((= sdl2-ffi::+sdl-windowevent-size-changed+ e)
			  (make-instance 'window-configuration-event
                                         :sheet sheet
                                         :x x :y y :width data1 :height data2))
			 (t
			  ;;(format *debug-io* "bo: ~a~%" e)
			  nil)))))))
              (:mousemotion
               (let* ((w (sdl2::c-ref event sdl2-ffi:sdl-event :motion :window-id))
		      (time (sdl2::c-ref event sdl2-ffi:sdl-event :motion :timestamp))
		      (x (sdl2::c-ref event sdl2-ffi:sdl-event :motion :x))	  
		      (y (sdl2::c-ref event sdl2-ffi:sdl-event :motion :y))
                      (xrel (sdl2::c-ref event sdl2-ffi:sdl-event :motion :xrel))	  
		      (yrel (sdl2::c-ref event sdl2-ffi:sdl-event :motion :yrel))
                      (which (sdl2::c-ref event sdl2-ffi:sdl-event :motion :which))
                      (state (sdl2::c-ref event sdl2-ffi:sdl-event :motion :state))
		      (sheet  (with-slots (window->sheet) port
				(gethash w window->sheet))))
		 ;;(format *debug-io* "Motion: ~A ~A ~A ~A ~A~%" w time sheet (list x y) (list xrel yrel))
                 (make-instance 'pointer-motion-event
                                :pointer which :button 0
                                :x x :y y
                                :graft-x x
                                :graft-y y
                                :sheet sheet
                                :modifier-state 0;;modifier-state
                                :timestamp time)))
              ((:mousebuttondown :mousebuttonup)
               (let* ((w (sdl2::c-ref event sdl2-ffi:sdl-event :button :window-id))
		      (time (sdl2::c-ref event sdl2-ffi:sdl-event :button :timestamp))
		      (x (sdl2::c-ref event sdl2-ffi:sdl-event :button :x))	  
		      (y (sdl2::c-ref event sdl2-ffi:sdl-event :button :y))
                      (button (sdl2::c-ref event sdl2-ffi:sdl-event :button :button))	  
                      (which (sdl2::c-ref event sdl2-ffi:sdl-event :button :which))
                      (state (sdl2::c-ref event sdl2-ffi:sdl-event :button :state))
		      (sheet  (with-slots (window->sheet) port
				(gethash w window->sheet))))
                 (make-instance (if (eq etype :mousebuttondown)
                                    'pointer-button-press-event
                                    'pointer-button-release-event)
                                :pointer which
                                :button button :x x :y y
                                :graft-x x
                                :graft-y y
                                :sheet sheet
                                :modifier-state 0 ;;modifier-state
                                :timestamp time)))
              (:mousewheel
               nil)
              (:keydown
               nil)
              (:keyup
               nil)
              (:textinput
               nil)
              (:quit
               (format *debug-io* "quit: ~a~%" event)
               nil)
              (t 
               ;;(format *debug-io* "event: ~a ~a~%" etype event)
               nil)))))))


(defmethod make-graft ((port sdl2-port) &key (orientation :default) (units :device))
  (let ((graft (make-instance 'sdl2-graft
			      :port port :mirror (gensym)
			      :orientation orientation :units units)))
    (push graft (port-grafts port))
    graft))

;;; the generic function port-character-width might be intended to be
;;; common for all ports, but in fact, that symbol is in the clim-sdl2
;;; package, so it is only defined here, and nowhere used.
#|
(defgeneric port-character-width (port text-style char))

(defmethod port-character-width ((port sdl2-port) text-style char)
  (declare (ignore text-style char))
  10)

;;; the generic function port-string-width might be intended to be
;;; common for all ports, but in fact, that symbol is in the clim-sdl2
;;; package, so it is only defined here, and nowhere used. 
(defgeneric port-string-width (port text-style string &key start end))

(defmethod port-string-width ((port sdl2-port) text-style string &key (start 0) end)
  (declare (ignore text-style string start end))
  100)
|#

(defmethod port-mirror-width ((port sdl2-port) sheet)
  (declare (ignore sheet))
  (break))
  

(defmethod port-mirror-height ((port sdl2-port) sheet)
  (declare (ignore sheet))
  (break))

(defmethod graft ((port sdl2-port))
  (first (climi::port-grafts port)))

(defun %destroy-all-mirrors (port)
  (maphash #'(lambda (key val)
	       (port-unregister-mirror port key val)
	       (destroy-mirror port key))
	   (slot-value port 'climi::sheet->mirror)))

(defmethod destroy-port :before ((port sdl2-port))
 (%destroy-all-mirrors port))

(defmethod pointer-position ((pointer sdl2-pointer))
  (values (slot-value pointer 'x) (slot-value pointer 'y)))

(defmethod pointer-button-state ((pointer sdl2-pointer))
  nil)

(defmethod port-modifier-state ((port sdl2-port))
  nil)

(defmethod synthesize-pointer-motion-event ((pointer sdl2-pointer))
  nil)

(defmethod port-frame-keyboard-input-focus ((port sdl2-port) frame)
  (frame-properties frame 'focus))

(defmethod (setf port-frame-keyboard-input-focus) 
    (focus (port sdl2-port) frame)
  (setf (frame-properties frame 'focus) focus))

(defmethod (setf port-keyboard-input-focus) (focus (port sdl2-port))
  focus)

(defmethod port-keyboard-input-focus ((port sdl2-port))
  nil)

(defmethod port-force-output ((port sdl2-port))
  nil)

;; fixme: what happens when clim code calls tracking-pointer recursively?
(defmethod port-grab-pointer ((port sdl2-port) pointer sheet)
  (declare (ignore pointer sheet))
  nil)

(defmethod port-ungrab-pointer ((port sdl2-port) pointer sheet)
  (declare (ignore pointer sheet))
  nil)

(defmethod distribute-event :around ((port sdl2-port) event)
  (declare (ignore event))
  ;;(format *debug-io* "distribute event: ~a ~%" event)
  (call-next-method))

(defmethod set-sheet-pointer-cursor ((port sdl2-port) sheet cursor)
  (declare (ignore sheet cursor))
  nil)        

(defmethod bind-selection ((port sdl2-port) window &optional time)
  (declare (ignore window time))
  nil)

(defmethod release-selection ((port sdl2-port) &optional time)
  (declare (ignore time))
  nil)

(defmethod request-selection ((port sdl2-port) requestor time)
  (declare (ignore requestor time))
  nil)

(defmethod get-selection-from-event ((port sdl2-port) event)
  (declare (ignore event))
  nil)

(defmethod send-selection ((port sdl2-port) event string)
  (declare (ignore event string))
  nil)

(defmethod allocate-space :after ((pane top-level-sheet-pane) width height)
  (when (sheet-direct-mirror pane)
    (with-slots (space-requirement) pane
      (format *debug-io* "ALLOCATE: ~a ~a ~a ~a ~%" pane (sheet-region pane) width height)
      
      (port-set-mirror-region (port pane)
			      (sheet-mirror pane)
      		      (make-rectangle* 0 0 width height))))
  (setf (sheet-region pane) (make-rectangle* 0 0 width height)))


(defmethod port-set-mirror-region ((port sdl2-port) mirror mirror-region)
  (with-slots (mirror->window) port
    (format *debug-io* "REGION ~A ~A~%" mirror mirror-region)
    (format *debug-io* "REGION ~A~%" (gethash mirror mirror->window))
    (let ((win (sdl2-ffi.functions::sdl-get-window-from-id (gethash mirror mirror->window))))
      (multiple-value-bind (w h)
	  (sdl2:get-window-size win)
	(unless (and (eql w (round (bounding-rectangle-max-x mirror-region)))
		     (eql h (round (bounding-rectangle-max-y mirror-region))))
	  (format *debug-io* "--- REGION ~A ~A ~A ~A~%" w h (round (bounding-rectangle-max-x mirror-region))
		  (round (bounding-rectangle-max-y mirror-region)))
	  (sdl2:set-window-size
	   win
	   (floor (round (bounding-rectangle-max-x mirror-region)))
	   (floor (round (bounding-rectangle-max-y mirror-region)))))))))

(defmethod port-set-mirror-transformation ((port sdl2-port) mirror mirror-transformation)
  (format *debug-io* "transformation ~a~%" mirror-transformation)
  ;;(call-next-method)
  (with-slots (mirror->window) port
    (let ((win (sdl2-ffi.functions::sdl-get-window-from-id (gethash mirror mirror->window))))
      (sdl2:set-window-position
       win
       (floor (nth-value 0 (transform-position mirror-transformation 0 0)))
       (floor (nth-value 1 (transform-position mirror-transformation 0 0)))))))

(defmethod port-enable-sheet ((port sdl2-port) (sheet mirrored-sheet-mixin))
  (with-slots (sheet->window) port
    (let ((win (gethash sheet sheet->window)))
      (format *debug-io* "SHOW: ~A ~A~%" win (<+ `(sdl2-ffi.functions::sdl-get-window-from-id ,win)))
      (<+ `(sdl2:show-window (sdl2-ffi.functions::sdl-get-window-from-id ,win))))))

(defmethod port-disable-sheet ((port sdl2-port) (mirror mirrored-sheet-mixin))
  (with-slots (sheet->window) port
    (let ((win (gethash sheet sheet->window)))
      (format *debug-io* "HIDE:~%")
      (<+ `(sdl2:hide-window (sdl2-ffi.functions::sdl-get-window-from-id ,win))))))

(defmethod destroy-port :before ((port sdl2-port))
  (close-sdl2-server))

#|
(defmethod mirror-transformation ((port sdl2-port) mirror)
  ())


(defmethod port-set-sheet-region ((port sdl2-port) (graft graft) region)
  (format *debug-io* "g region ~a~%" region))

(defmethod port-set-sheet-transformation
    ((port sdl2-port) (graft graft) transformation)
  (format *debug-io* "g transf ~a~%" region))
  

(defmethod port-set-sheet-transformation
    ((port sdl2-port) (sheet mirrored-sheet-mixin) transformation)
  (format *debug-io* "s trans ~a~%" region))

(defmethod port-set-sheet-region
    ((port sdl2-port) (sheet mirrored-sheet-mixin) region)
  (declare (ignore region))
  (format *debug-io* "s region ~a~%" region))
  




(defmethod port-motion-hints ((port sdl2-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod (setf port-motion-hints)
    (value (port sdl2-port) (sheet mirrored-sheet-mixin))
  value)
|#


(defmethod synthesize-pointer-motion-event ((pointer sdl2-pointer))
  #|
  (let* ((port (port pointer))
	 (sheet (port-pointer-sheet port)))
    (when sheet
      (let ((mirror (sheet-direct-xmirror sheet)))
	(when mirror
	  (multiple-value-bind (x y same-screen-p child mask root-x root-y)
	      (xlib:query-pointer mirror)
	    (declare (ignore child))
	    (when same-screen-p
	      (make-instance
	       'pointer-motion-event
	       :pointer 0 :button (button-from-state mask)
	       :x x :y y
	       :graft-x root-x
	       :graft-y root-y
	       :sheet sheet
	       :modifier-state (clim-xcommon:x-event-state-modifiers port mask)
	       ;; The event initialization code will give us a
	       ;; reasonable timestamp.
	       :timestamp 0))))))))
  |#
  )

(defmethod fb-port-realize-pixmap ((port sdl2-port) sheet width height)
  (make-instance 'fb-pixmap
                 :sheet sheet
                 :width width
                 :height height
                 :port port))
