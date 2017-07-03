(in-package :clim-sdl2)

(defvar *sdl2-server*  nil)

(defvar *sdl2-server-command-queue*
  (lparallel.queue:make-queue))

(defvar *sdl2-server-event-queue*
  (lparallel.queue:make-queue))

(defparameter *sdl2-server-debug* t)

;; This must be set, otherwise because thread bindings, directly print to *standard-output* will not display in SLIME.
(defparameter *sdl2-server-debug-output* *standard-output*)

(defparameter *debug-print-lock* (bt:make-recursive-lock "DEBUG-PRINT-lock"))

(defun debug-print(&rest args)
  (bt:with-recursive-lock-held (*debug-print-lock*)
    (when *sdl2-server-debug*
      (format *sdl2-server-debug-output* "~{~a~%~}" args))))

(defun debug-prin1 (&rest args)
  (bt:with-recursive-lock-held (*debug-print-lock*)
    (when *sdl2-server-debug*
      (format *sdl2-server-debug-output* "~{~a ~}~%" args))))

(defun start-sdl2-server (port)
  (or *sdl2-server*
      (setf *sdl2-server*
	    (bt:make-thread #'(lambda ()
				(sdl2-server-loop port))
			    :name "sdl2-server"))))

(defun empty-queue (queue)
  (loop
     (if (lparallel.queue:queue-empty-p queue)
	 (return)
	 (lparallel.queue:pop-queue queue))))

(defun close-sdl2-server ()
  (when *sdl2-server*
    (<+ `(sdl2:quit))
    (bt:destroy-thread *sdl2-server*)
    (setf *sdl2-server* nil)
    (empty-queue *sdl2-server-command-queue*)
    (empty-queue *sdl2-server-event-queue*)))

(defun restart-sdl2-server ()
  (close-sdl2-server)
  (start-sdl2-server))

(defun sdl2-server-loop (port)
  ;;(sdl2:init :everything)
   (let ((init-flags (autowrap:mask-apply 'sdl2::sdl-init-flags '(:everything))))
       (sdl2::sdl-init init-flags))
  (loop
     (with-simple-restart
	 (restart-event-loop
	  "restart clim's event loop.")
       (loop
	  (process-next-event port :timeout 0.01)
	  (let ((command-and-promise (sdl2-server-listen)))
	    (when command-and-promise
	      (sdl2-server-read)
	      (lparallel:fulfill (cdr command-and-promise)
		(process-command (car command-and-promise)))))
          (handler-case
              (fb-port-flush-mirrors port)
            (condition (condition)
              (format *debug-io* "~A~%" condition)))))))

#|
  (cffi:with-foreign-object (msg-ptr '(:struct gfs::msg))
    (loop
       (let ((gm (gfs::peek-message msg-ptr (cffi:null-pointer) 0 0 (logior gfs::+pm-noyield+ gfs::+pm-remove+)))
	     (command-and-promise (sdl2-server-listen)))
	 (when command-and-promise
	   (sdl2-server-read)
	   (lparallel:fulfill (cdr command-and-promise)
	     (process-command (car command-and-promise))))
	 (when (/= gm 0)
	  (cffi:with-foreign-slots ((gfs::message gfs::wparam) msg-ptr (:struct gfs::msg))
	    (when (funcall 'gfw:default-message-filter gm msg-ptr)
	      (return-from sdl2-server-loop gfs::wparam))))))))
|#


(defun sdl2-server-listen ()
  (lparallel.queue:peek-queue *sdl2-server-command-queue*))

(defun sdl2-server-read ()
  (lparallel.queue:pop-queue *sdl2-server-command-queue*))

(defun send-to-sdl2-server (command)
  (assert *sdl2-server*)
  (let ((p (lparallel:promise)))
    (lparallel.queue:push-queue (cons command p) *sdl2-server-command-queue*)
    p))

(defun send-to-sdl2-server/blocked (command)
  (lparallel:force (send-to-sdl2-server command)))

(defmacro <- (command)
  `(send-to-sdl2-server ,command))

(defmacro <+ (command)
  `(send-to-sdl2-server/blocked ,command))

;;; todo: need to examine command and recover from error
;;; there must be no blocked command send in process-command
(defun process-command (command)
  (format *debug-io* "con: ~a~%" command)
  (let ((result
	 ;;(sdl2:in-main-thread (:no-event t)
	 (eval command)
	  ;;)))
	 ))
    (format *debug-io* "result: ~a~%" result)
    result))
  #|
  (case (first command)
    (gfw:show
     (gfw:show (second command) (third command)))
    (otherwise (let ((result (eval command)))
		 result))))
  |#
 



(defun server-add-event (event)
  ;;(setf (slot-value event 'climi::timestamp) (gfw:obtain-event-time))
  (lparallel.queue:push-queue event *sdl2-server-event-queue*))

(defun server-get-event ()
  (lparallel.queue:pop-queue *sdl2-server-event-queue*))



(defun close-sdl2-server ()
  (when *sdl2-server*
    (<+ `(sdl2:sdl-quit))
    (bt:destroy-thread *sdl2-server*)
    (setf *sdl2-server* nil)
    (empty-queue *sdl2-server-command-queue*)
    (empty-queue *sdl2-server-event-queue*)))
