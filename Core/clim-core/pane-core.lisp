(in-package :clim-internals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Standard Space Requirements

(defun make-space-requirement (&key (min-width 0) (min-height 0)
                                 (width min-width) (height min-height)
                                 (max-width +fill+) (max-height +fill+))
  ;; Defensive programming. For instance SPACE-REQUIREMENT-+ may cause
  ;; max-{width,height} to be (+ +fill+ +fill+), what exceeds our biggest
  ;; allowed values. We fix that here.
  (clampf min-width  0 +fill+) (clampf max-width  0 +fill+) (clampf width  min-width  max-width)
  (clampf min-height 0 +fill+) (clampf max-height 0 +fill+) (clampf height min-height max-height)
  (assert (<= min-width  max-width)  (min-width  max-width))
  (assert (<= min-height max-height) (min-height max-height))
  (make-instance 'standard-space-requirement
                 :width width
                 :max-width max-width
                 :min-width min-width
                 :height height
                 :max-height max-height
                 :min-height min-height))

(defmethod space-requirement-components ((space-req standard-space-requirement))
  (with-slots (width min-width max-width height min-height max-height) space-req
    (values width min-width max-width height min-height max-height)))

(defmethod print-object ((space standard-space-requirement) stream)
  (with-slots (width height min-width max-width min-height max-height) space
    (print-unreadable-object (space stream :type t :identity nil)
      (format stream "width: ~S [~S,~S] height: ~S [~S,~S]"
              width
              min-width
              max-width
              height
              min-height
              max-height))))

(defun space-requirement-combine* (function sr1 &key (width 0) (min-width 0) (max-width 0)
                                                (height 0) (min-height 0) (max-height 0))
  (apply #'make-space-requirement
         (mapcan #'(lambda (c1 c2 keyword)
                     (list keyword (funcall function c1 c2)))
                 (multiple-value-list (space-requirement-components sr1))
                 (list width min-width max-width height min-height max-height)
                 '(:width :min-width :max-width :height :min-height :max-height))))

(defun space-requirement-combine (function sr1 sr2)
  (multiple-value-bind (width min-width max-width height min-height max-height)
      (space-requirement-components sr2)
    (space-requirement-combine* function sr1
                                :width      width
                                :min-width  min-width
                                :max-width  max-width
                                :height     height
                                :min-height min-height
                                :max-height max-height)))

(defun space-requirement+ (sr1 sr2)
  (space-requirement-combine #'+ sr1 sr2))

(defun space-requirement+* (space-req &key (width 0) (min-width 0) (max-width 0)
                                           (height 0) (min-height 0) (max-height 0))
  (space-requirement-combine* #'+ space-req
                              :width      width
                              :min-width  min-width
                              :max-width  max-width
                              :height     height
                              :min-height min-height
                              :max-height max-height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; User Space Requirements

(defmethod shared-initialize :after ((instance space-requirement-options-mixin)
                                     (slot-names t)
                                     &key
                                     (x-spacing nil x-spacing-p)
                                     (y-spacing nil y-spacing-p)
                                     (spacing nil spacing-p))
  (declare (ignore x-spacing y-spacing))
  (cond ((not spacing-p))
        (x-spacing-p
         (error #1="~@<The initargs ~S and ~S are mutually exclusive~@:>"
                :spacing :x-spacing))
        (y-spacing-p
         (error #1# :spacing :y-spacing))
        (t
         (setf (slot-value instance 'x-spacing) spacing
               (slot-value instance 'y-spacing) spacing))))

(defmethod change-space-requirements :before ((pane space-requirement-options-mixin)
                                              &key (width :nochange) (min-width :nochange)
                                                (max-width :nochange)
                                              (height :nochange) (min-height :nochange)
                                                (max-height :nochange)
                                                (x-spacing :nochange) (y-spacing :nochange)
                                                &allow-other-keys)
  (with-slots (user-width user-min-width user-max-width
                          user-height user-min-height user-max-height
                          (user-x-spacing x-spacing)
                          (user-y-spacing y-spacing))
      pane
    (unless (eq width      :nochange) (setf user-width      width))
    (unless (eq min-width  :nochange) (setf user-min-width  min-width))
    (unless (eq max-width  :nochange) (setf user-max-width  max-width))
    (unless (eq height     :nochange) (setf user-height     height))
    (unless (eq min-height :nochange) (setf user-min-height min-height))
    (unless (eq max-height :nochange) (setf user-max-height max-height))
    (unless (eq x-spacing  :nochange) (setf user-x-spacing  x-spacing))
    (unless (eq y-spacing  :nochange) (setf user-y-spacing  y-spacing)) ))

(defun merge-one-option
    (pane foo user-foo user-min-foo user-max-foo min-foo max-foo)


  ;; NOTE: The defaulting for :min-foo and :max-foo is different from MAKE-SPACE-REQUIREMENT.
  ;;       MAKE-SPACE-REQUIREMENT has kind of &key foo (min-foo 0) (max-foo +fill+)
  ;;       While user space requirements has &key foo (min-foo foo) (max-foo foo).
  ;;       I as a user would pretty much expect the same behavior, therefore I'll take the
  ;;       following route:
  ;;       When the :foo option is given, I'll let MAKE-SPACE-REQUIREMENT decide.
  ;;
  ;; old code:
  ;;
  ;; ;; Then we resolve defaulting. sec 29.3.1 says:
  ;; ;; | If either of the :max-width or :min-width options is not
  ;; ;; | supplied, it defaults to the value of the :width option. If
  ;; ;; | either of the :max-height or :min-height options is not
  ;; ;; | supplied, it defaults to the value of the :height option.
  ;; (setf user-max-foo  (or user-max-foo user-foo)
  ;;       user-min-foo  (or user-min-foo user-foo))
  ;;       --GB 2003-01-23

  (when (and (null user-max-foo) (not (null user-foo)))
    (setf user-max-foo (space-requirement-max-width
			(make-space-requirement
			 :width (spacing-value-to-device-units pane foo)))))
  (when (and (null user-min-foo) (not (null user-foo)))
    (setf user-min-foo (space-requirement-min-width
			(make-space-requirement
			 :width (spacing-value-to-device-units pane foo)))))

  ;; when the user has no idea about the preferred size just take the
  ;; panes preferred size.
  (setf user-foo (or user-foo foo))
  (setf user-foo (spacing-value-to-device-units pane user-foo))

  ;; dito for min/max
  (setf user-min-foo (or user-min-foo min-foo)
	user-max-foo (or user-max-foo max-foo))

  ;; | :max-width, :min-width, :max-height, and :min-height can
  ;; | also be specified as a relative size by supplying a list of
  ;; | the form (number :relative). In this case, the number
  ;; | indicates the number of device units that the pane is
  ;; | willing to stretch or shrink.
  (labels ((resolve-relative (dimension sign base)
	     (if (and (consp dimension) (eq (car dimension) :relative))
		 (+ base (* sign (cadr dimension)))
		 (spacing-value-to-device-units pane dimension))))
    (setf user-min-foo (and user-min-foo
			    (resolve-relative user-min-foo  -1 user-foo))
	  user-max-foo (and user-max-foo
			    (resolve-relative user-max-foo  +1 user-foo))))

  ;; Now we have two space requirements which need to be 'merged'.
  (setf min-foo (clamp user-min-foo min-foo max-foo)
	max-foo (clamp user-max-foo min-foo max-foo)
	foo     (clamp user-foo min-foo max-foo))
  (values foo min-foo max-foo))

(defmethod merge-user-specified-options ((pane space-requirement-options-mixin)
					 sr)
  ;; ### I want proper error checking and in case there is an error we
  ;;     should just emit a warning and move on. CLIM should not die from
  ;;     garbage passed in here.
  (multiple-value-bind (width min-width max-width height min-height max-height)
		       (space-requirement-components sr)
    (multiple-value-bind (new-width new-min-width new-max-width)
	(merge-one-option pane
			  width
			  (pane-user-width pane)
			  (pane-user-min-width pane)
			  (pane-user-max-width pane)
			  min-width
			  max-width)
      (multiple-value-bind (new-height new-min-height new-max-height)
	  (merge-one-option pane
			    height
			    (pane-user-height pane)
			    (pane-user-min-height pane)
			    (pane-user-max-height pane)
			    min-height
			    max-height)
	(make-space-requirement
	 :width      new-width
	 :min-width  new-min-width
	 :max-width  new-max-width
	 :height     new-height
	 :min-height new-min-height
	 :max-height new-max-height)))))

;;;
;;; compose space
;;;

(defmethod compose-space :around ((pane space-requirement-options-mixin)
                                  &key width height)
  (declare (ignore width height))
  ;; merge user specified options.
  (let ((sr (call-next-method)))
    (unless sr
      (warn "~S has no idea about its space-requirements." pane)
      (setf sr (make-space-requirement :width 100 :height 100)))
    (merge-user-specified-options pane sr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Layout
(defmethod note-sheet-grafted ((pane top-level-sheet-pane-mixin))
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
    (setf (pane-current-width pane) (- x2 x1)
	  (pane-current-height pane) (- y2 y1))
    (allocate-space pane (- x2 x1) (- y2 y1))))

(defmethod note-sheet-region-changed :after ((pane sheet-mixin) old-region)
  (declare (ignore old-region))
  (with-bounding-rectangle* (x1 y1 x2 y2)
    (sheet-region pane)
    (when (not (and (eql (pane-current-width pane) (- x2 x1))
	          (eql (pane-current-height pane) (- y2 y1))))
        (setf (pane-current-width pane) (- x2 x1)
	      (pane-current-height pane) (- y2 y1))
        (allocate-space pane (- x2 x1) (- y2 y1)))))

(defmethod allocate-space :around ((pane mirrored-sheet-mixin) width height)
  (if (and (eql (pane-current-width pane) width)
	   (eql (pane-current-height pane) height))
      (progn
        (log:info "ralloc mirror ~A ~A !!" width height)
        (call-next-method))
      (progn
        (log:info "resize mirror ~A ~A !!" width height)
        (request-resize-sheet pane width height))))

(defmethod allocate-space :around ((pane sheet-mixin) width height)
  (if (and (eql (pane-current-width pane) width)
	   (eql (pane-current-height pane) height))
      (progn
        #+nil (log:info "ralloc ~A !!" pane)
        (call-next-method))
      (progn
        (log:info "resize ~A !!" pane)
        (resize-sheet pane width height))))

(defmethod compose-space :around ((pane layout-protocol-mixin) &key width height)
  (declare (ignore width height))
  (or (pane-space-requirement pane)
      (setf (pane-space-requirement pane)
            (call-next-method))))
