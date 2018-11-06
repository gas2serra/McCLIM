(in-package :clim-internals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; enabled/disabled

(defmethod (setf sheet-enabled-p) :around (enabled-p (sheet sheet-mixin))
  (unless (eql enabled-p (sheet-enabled-p sheet))
    (call-next-method)
    (if enabled-p
        (note-sheet-enabled sheet)
        (note-sheet-disabled sheet))
    (awhen (sheet-parent sheet)
      (if (sheet-enabled-p sheet)
          (note-successor-enabled it sheet)
          (note-successor-disabled it sheet)))))

(defmethod sheet-enabled-children ((sheet sheet-mixin))
  (delete-if-not #'sheet-enabled-p (copy-list (sheet-children sheet))))

(defmethod sheet-viewable-p ((sheet sheet-mixin))
  (and (sheet-parent sheet)
       (sheet-enabled-p sheet)
       (sheet-viewable-p (sheet-parent sheet))))

(defmethod sheet-viewable-p ((graft sheet-graft-mixin))
  (sheet-enabled-p graft))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; relations: query

(defmethod sheet-parent ((sheet sheet-mixin))
  nil)

(defmethod sheet-children ((sheet sheet-mixin))
  nil)

(defmethod sheet-children ((sheet sheet-single-child-mixin))
  (and (sheet-child sheet) (list (sheet-child sheet))))

(defmethod sheet-children ((sheet sheet-leaf-mixin))
  nil)

(defmethod sheet-siblings ((sheet sheet-mixin))
  (when (not (sheet-parent sheet))
    (error 'sheet-is-not-child))
  (remove sheet (sheet-children (sheet-parent sheet))))

(defmethod sheet-ancestor-p ((sheet sheet-mixin)
			     (putative-ancestor sheet))
  (or (eq sheet putative-ancestor)
      (and (sheet-parent sheet)
	   (sheet-ancestor-p (sheet-parent sheet) putative-ancestor))))

(defmethod map-over-sheets (function (sheet sheet-mixin))
  (funcall function sheet)
  (map nil
       #'(lambda (child)
           (map-over-sheets function child))
       (sheet-children sheet)))

(defmethod map-over-sheets (function (sheets list))
  (map nil
       #'(lambda (child)
           (map-over-sheets function child))
       sheets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; relations: build

(defmethod sheet-adopt-child ((sheet sheet-mixin) (child sheet))
  (error "~S attempting to adopt ~S" sheet child))

(defmethod sheet-adopt-child :after ((sheet sheet-mixin) (child sheet-mixin))
  (note-sheet-adopted child)
  (note-successor-adopted sheet child)
  (when (sheet-grafted-p sheet)
    (note-sheet-grafted child)))

(defmethod sheet-adopt-child :before (sheet (child sheet-parent-mixin))
  (when (sheet-parent child)
    (error 'sheet-already-has-parent))
  (when (sheet-ancestor-p sheet child)
    (error 'sheet-is-ancestor)))

(defmethod sheet-adopt-child :after (sheet (child sheet-parent-mixin))
  (setf (sheet-parent child) sheet))

(defmethod sheet-adopt-child :before ((sheet sheet-single-child-mixin)
				      (child sheet-parent-mixin))
  (when (sheet-child sheet)
    (error 'sheet-supports-only-one-child :sheet sheet)))

(defmethod sheet-adopt-child ((sheet sheet-single-child-mixin)
			      (child sheet-parent-mixin))
  (setf (sheet-child sheet) child))

(defmethod sheet-adopt-child ((sheet sheet-multiple-child-mixin)
			      (child sheet-parent-mixin))
  (push child (sheet-children sheet)))

(defmethod sheet-adopt-child ((sheet sheet-leaf-mixin) (child sheet))
  (error "Leaf sheet attempting to adopt a child"))

(defmethod sheet-disown-child ((sheet sheet-mixin) (child sheet) &key (errorp t))
  (declare (ignore errorp))
  (error "~S attempting to disown ~S" sheet child))

(defmethod sheet-disown-child :after
    ((sheet sheet-mixin) (child sheet-mixin) &key (errorp t))
  (declare (ignore errorp))
  (note-sheet-disowned child)
  (note-successor-adopted sheet child)  
  (when (sheet-grafted-p sheet)
    (note-sheet-degrafted child)))

(defmethod sheet-disown-child :before
    ((sheet sheet-mixin) (child sheet) &key (errorp t))
  (when (and (not (member child (sheet-children sheet))) errorp)
    (error 'sheet-is-not-child)))

(defmethod sheet-disown-child :after (sheet
				      (child sheet-parent-mixin)
				      &key (errorp t))
  (declare (ignore sheet errorp))
  (setf (sheet-parent child) nil))


(defmethod sheet-disown-child ((sheet sheet-single-child-mixin)
			       (child sheet-parent-mixin)
			       &key (errorp t))
  (declare (ignore errorp))
  (setf (sheet-child sheet) nil))

(defmethod sheet-disown-child ((sheet sheet-multiple-child-mixin)
			       (child sheet-parent-mixin)
			       &key (errorp t))
  (declare (ignore errorp))
  (setf (sheet-children sheet) (delete child (sheet-children sheet))))

(defmethod sheet-disown-child
    ((sheet sheet-leaf-mixin) (child sheet) &key (errorp t))
  (declare (ignorable errorp))
  (error "Leaf sheet attempting to disown a child"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; relations: order

(defmethod raise-sheet ((sheet sheet-mixin))
  (error 'sheet-is-not-child))

(defmethod raise-sheet ((sheet sheet-parent-mixin))
  (awhen (sheet-parent sheet)
    (reorder-sheets it
                    (cons sheet (remove sheet (sheet-children it))))))

(defmethod bury-sheet ((sheet sheet-mixin))
  (error 'sheet-is-not-child))

(defmethod bury-sheet ((sheet sheet-parent-mixin))
  (awhen (sheet-parent sheet)
    (reorder-sheets it
                    (append (remove sheet (sheet-children it)) (list  sheet)))))

(defmethod reorder-sheets ((sheet sheet-mixin) new-ordering)
  (error 'sheet-is-not-child))

(defmethod reorder-sheets :around ((sheet sheet-multiple-child-mixin) new-ordering)
  (let ((old-order (copy-list (sheet-children sheet))))
    (call-next-method)
    (note-children-order-changed sheet old-order)
    (awhen (sheet-parent sheet)
      (note-successor-order-changed it sheet old-order))))

(defmethod reorder-sheets ((sheet sheet-multiple-child-mixin) new-ordering)
  (when (set-difference (sheet-children sheet) new-ordering)
    (error 'sheet-ordering-underspecified))
  (when (set-difference new-ordering (sheet-children sheet))
    (error 'sheet-is-not-child))
  (setf (sheet-children sheet) new-ordering)
  sheet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; geometry: transformation

(defmethod sheet-transformation ((sheet sheet-mixin))
  (error "Attempting to get the TRANSFORMATION of a SHEET that doesn't contain one"))

(defmethod sheet-transformation ((sheet sheet-identity-transformation-mixin))
  +identity-transformation+)

(defmethod (setf sheet-transformation) (transformation (sheet sheet-mixin))
  (declare (ignore transformation))
  (error "Attempting to set the TRANSFORMATION of a SHEET that doesn't contain one"))

(defmethod (setf sheet-transformation) :around (transformation (sheet sheet-mixin))
  (let ((old-transformation (sheet-transformation sheet)))
    (call-next-method)
    (unless (transformation-equal old-transformation transformation)
      (note-sheet-transformation-changed sheet old-transformation)
      (awhen (sheet-parent sheet)
             (note-successor-transformation-changed it sheet old-transformation)))))

(defmethod (setf sheet-transformation) :before
    ((transformation transformation)
     (sheet sheet-translation-transformation-mixin))
  (unless (translation-transformation-p transformation)
    (error "Attempting to set the SHEET-TRANSFORMATION of a SHEET-TRANSLATION-TRANSFORMATION-MIXIN to a non translation transformation")))

(defmethod (setf sheet-transformation) :before
    ((transformation transformation)
     (sheet sheet-y-inverting-transformation-mixin))
  (unless (y-inverting-transformation-p transformation)
    (error "Attempting to set the SHEET-TRANSFORMATION of a SHEET-Y-INVERTING-TRANSFORMATION-MIXIN to a non Y inverting transformation")))

(defmethod sheet-delta-transformation ((sheet sheet-mixin) (ancestor (eql nil)))
  (cond ((sheet-parent sheet)
	 (compose-transformations (sheet-transformation sheet)
				  (sheet-delta-transformation
				   (sheet-parent sheet) ancestor)))
	(t +identity-transformation+)))

(defmethod sheet-delta-transformation ((sheet sheet-mixin) (ancestor sheet))
  (cond ((eq sheet ancestor) +identity-transformation+)
	((sheet-parent sheet)
	 (compose-transformations (sheet-transformation sheet)
				  (sheet-delta-transformation
				   (sheet-parent sheet) ancestor)))
	(t (error 'sheet-is-not-ancestor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; geometry: region


(defmethod (setf sheet-region) :around (region (sheet sheet-mixin))
  (let ((old-region (sheet-region sheet)))
    (call-next-method)
    (unless (region-equal old-region region)
      (note-sheet-region-changed sheet old-region)
      (awhen (sheet-parent sheet)
             (note-successor-region-changed it sheet old-region)))))

(defmethod move-sheet ((sheet sheet-mixin) x y)
  (let ((transform (sheet-transformation sheet)))
    (multiple-value-bind (old-x old-y)
        (transform-position transform 0 0)
      (let ((dx (- x old-x))
            (dy (- y old-y)))
        (unless (and (zerop dx) (zerop dy))
          (setf (sheet-transformation sheet)
                (compose-translation-with-transformation
                 transform (- x old-x) (- y old-y))))))))

(defmethod resize-sheet ((sheet sheet-mixin) width height)
  (setf (sheet-region sheet)
        (make-bounding-rectangle 0 0 width height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; geometry: transformation query

(defmethod map-sheet-position-to-parent ((sheet sheet-mixin) x y)
  (declare (ignore x y))
  (error "Sheet has no parent"))

(defmethod map-sheet-position-to-child ((sheet sheet-mixin) x y)
  (declare (ignore x y))
  (error "Sheet has no parent"))

(defmethod map-sheet-rectangle*-to-parent ((sheet sheet-mixin) x1 y1 x2 y2)
  (declare (ignore x1 y1 x2 y2))
  (error "Sheet has no parent"))

(defmethod map-sheet-rectangle*-to-child ((sheet sheet-mixin) x1 y1 x2 y2)
  (declare (ignore x1 y1 x2 y2))
  (error "Sheet has no parent"))

(defmethod map-sheet-position-to-parent ((sheet sheet-parent-mixin) x y)
  (transform-position (sheet-transformation sheet) x y))

(defmethod map-sheet-position-to-child ((sheet sheet-parent-mixin) x y)
  (untransform-position (sheet-transformation sheet) x y))

(defmethod map-sheet-rectangle*-to-parent
    ((sheet sheet-parent-mixin) x1 y1 x2 y2)
  (transform-rectangle* (sheet-transformation sheet) x1 y1 x2 y2))

(defmethod map-sheet-rectangle*-to-child
    ((sheet sheet-parent-mixin) x1 y1 x2 y2)
  (untransform-rectangle* (sheet-transformation sheet) x1 y1 x2 y2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; geometry: region query

(defmethod sheet-occluding-sheets ((sheet sheet-mixin) (child sheet))
  (labels ((sheet-region-in-parent (sheet)
             (transform-region (sheet-transformation sheet)
                               (sheet-region sheet)))
           (fun (l)
		(cond ((eq (car l) child) '())
		      ((and (sheet-enabled-p (car l))
                            (region-intersects-region-p
                             (sheet-region-in-parent (car l))
                             (sheet-region-in-parent child)))
		       (cons (car l) (fun (cdr l))))
		      (t (fun (cdr l))))))
    (fun (sheet-children sheet))))

(defmethod map-over-sheets-containing-position (function (sheet sheet-mixin) x y)
  (map () #'(lambda (child)
              (multiple-value-bind (tx ty) (map-sheet-position-to-child child x y)
                (when (region-contains-position-p (sheet-region child) tx ty)
                  (funcall function child))))
       (sheet-children sheet)))

(defmethod map-over-sheets-overlapping-region (function (sheet sheet-mixin) region)
  (map () #'(lambda (child)
              (when (region-intersects-region-p
                     region
                     (transform-region
                      (if (eq child sheet)
                          +identity-transformation+
                          (sheet-transformation child))
                      (sheet-region child)))
                (funcall function child)))
       (sheet-children sheet)))

(defmethod child-containing-position ((sheet sheet-mixin) x y)
  (loop for child in (sheet-children sheet)
	do (multiple-value-bind (tx ty) (map-sheet-position-to-child child x y)
	     (when (and (sheet-enabled-p child)
			(region-contains-position-p (sheet-region child) tx ty))
	       (return child)))))

(defmethod children-overlapping-region ((sheet sheet-mixin) (region region))
  (loop for child in (sheet-children sheet)
	if (and (sheet-enabled-p child)
		(region-intersects-region-p
		 region
		 (transform-region (sheet-transformation child)
				   (sheet-region child))))
	  collect child))

(defmethod children-overlapping-rectangle* ((sheet sheet-mixin) x1 y1 x2 y2)
  (children-overlapping-region sheet (make-rectangle* x1 y1 x2 y2)))

(defmethod sheet-allocated-region ((sheet sheet-mixin) (child sheet))
  (reduce #'region-difference
	  (mapcar #'(lambda (child)
                      (transform-region (sheet-transformation child)
                                        (sheet-region child)))
                  (cons child (sheet-occluding-sheets sheet child)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; geometry: region native/device query

(defmethod sheet-native-region ((sheet sheet-mixin))
  (let ((this-native-region (transform-region
			     (sheet-native-transformation sheet)
			     (sheet-region sheet))))
    (aif (sheet-parent sheet)
         (region-intersection this-native-region
			      (sheet-native-region it))
	 this-native-region)))

(defmethod sheet-native-transformation ((sheet sheet-mixin))
  (aif (sheet-parent sheet)
       (compose-transformations
        (sheet-native-transformation it)
        (sheet-transformation sheet))
       +identity-transformation+))

(defmethod sheet-device-transformation ((sheet sheet-mixin))
  (let ((medium (sheet-medium sheet)))
    (compose-transformations
     (sheet-native-transformation sheet)
     (if medium
         (medium-transformation medium)
         +identity-transformation+))))

(defmethod sheet-device-region ((sheet sheet-mixin))
  (let ((medium (sheet-medium sheet)))
    (region-intersection
     (sheet-native-region sheet)
     (if medium
         (transform-region
          (sheet-device-transformation sheet)
          (medium-clipping-region medium))
         +everywhere+))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; cached sheet

(defmethod sheet-native-region :around ((sheet sheet-cached-mixin))
  (with-slots (cached-native-region) sheet
    (unless cached-native-region
      (setf cached-native-region (call-next-method)))
    cached-native-region))

(defmethod sheet-native-transformation :around ((sheet sheet-cached-mixin))
  (with-slots (cached-native-transformation) sheet
    (unless cached-native-transformation
      (setf cached-native-transformation (call-next-method)))
    cached-native-transformation))

(defmethod sheet-device-transformation :around ((sheet sheet-cached-mixin))
  (with-slots (cached-device-transformation) sheet
    (unless cached-device-transformation
      (setf cached-device-transformation (call-next-method)))
    cached-device-transformation))

(defmethod sheet-device-region :around ((sheet sheet-cached-mixin))
  (with-slots (cached-device-region) sheet
    (unless cached-device-region
      (setf cached-device-region (call-next-method)))
    cached-device-region))

(defmethod (setf sheet-transformation) :after (transformation (sheet sheet-cached-mixin))
  (declare (ignore transformation))
  (invalidate-cached-transformations sheet)
  (invalidate-cached-regions sheet))

(defmethod (setf sheet-region) :after (region (sheet sheet-cached-mixin))
  (declare (ignore region))
  (invalidate-cached-regions sheet))

(defmethod invalidate-cached-regions ((sheet sheet-cached-mixin))
  (with-slots (cached-native-region cached-device-region) sheet
    (setf cached-native-region nil)
    (setf cached-device-region nil))
  (mapc #'invalidate-cached-regions (sheet-children sheet)))

(defmethod invalidate-cached-transformations ((sheet sheet-cached-mixin))
  (with-slots (cached-native-transformation cached-device-transformation) sheet
    (setf cached-native-transformation nil)
    (setf cached-device-transformation nil))
  (mapc #'invalidate-cached-transformations (sheet-children sheet)))

(defmethod invalidate-cached-device-transformations ((sheet sheet-cached-mixin))
  (with-slots (cached-device-transformation) sheet
    (setf cached-device-transformation nil))
  (mapc #'invalidate-cached-device-transformations (sheet-children sheet)))

(defmethod invalidate-cached-device-regions ((sheet sheet-cached-mixin))
  (with-slots (cached-device-region) sheet
    (setf cached-device-region nil))
  (mapc #'invalidate-cached-device-regions (sheet-children sheet)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mirror protocols

(defmethod (setf sheet-enabled-p) :after
    (new-value (sheet mirrored-sheet-mixin))
  (when (and (sheet-direct-mirror sheet)
             (not (eql *configuration-event-p* sheet)))
    (if new-value
        (port-enable-sheet (port sheet) sheet)
        (port-disable-sheet (port sheet) sheet))))

(defmethod request-change-sheet-transformation ((sheet mirrored-sheet-mixin) transformation)
  (if (sheet-direct-mirror sheet)
      (port-set-mirror-transformation (port sheet)
                                      (sheet-direct-mirror sheet) transformation)
      (setf (sheet-transformation sheet) transformation)))

(defmethod request-change-sheet-region ((sheet mirrored-sheet-mixin) region)
  (if (sheet-direct-mirror sheet)
      (port-set-mirror-region (port sheet) (sheet-direct-mirror sheet) region)
      (setf (sheet-region sheet) region)))

(defmethod request-move-sheet ((sheet mirrored-sheet-mixin) x y)
  (if (sheet-direct-mirror sheet)
      (let ((transform (sheet-transformation sheet)))
        (multiple-value-bind (old-x old-y)
            (transform-position transform 0 0)
          (let ((dx (- x old-x))
                (dy (- y old-y)))
            (unless (and (zerop dx) (zerop dy))
              (request-change-sheet-transformation sheet
                                                   (compose-translation-with-transformation
                                                    transform (- x old-x) (- y old-y)))))))
      (move-sheet sheet x y)))

(defmethod request-resize-sheet ((sheet mirrored-sheet-mixin) width height)
  (if (sheet-direct-mirror sheet)
      (request-change-sheet-region sheet
                                   (make-bounding-rectangle 0 0 width height))
      (resize-sheet sheet width height)))

(defmethod (setf sheet-transformation) :around (transformation (sheet mirrored-sheet-mixin))
  (if (and (sheet-direct-mirror sheet)
           (not (eql *configuration-event-p* sheet)))
      (error "Attempting to set mirrored sheet transformation ~S; use request method" sheet)
      (call-next-method)))

(defmethod (setf sheet-region) :around (region (sheet mirrored-sheet-mixin))
  (if (and (sheet-direct-mirror sheet)
           (not (eql *configuration-event-p* sheet)))
      (error "Attempting to set mirrored sheet region ~S; use request method" sheet)
      (call-next-method)))

(defmethod raise-sheet :after ((sheet mirrored-sheet-mixin))
  (when (sheet-direct-mirror sheet)
    (raise-mirror (port sheet) sheet)))

(defmethod bury-sheet :after ((sheet mirrored-sheet-mixin))
  (when (sheet-direct-mirror sheet)
    (bury-mirror (port sheet) sheet)))

(defmethod note-sheet-grafted :before ((sheet mirrored-sheet-mixin))
  (unless (port sheet)
    (error "~S called on sheet ~S, which has no port?!" 'note-sheet-grafted sheet))
  (setf (%sheet-mirror-region sheet) (sheet-region sheet))
  (setf (%sheet-mirror-transformation sheet) (sheet-transformation sheet))
  (port-realize-mirror (port sheet) sheet)
  (setf (%sheet-mirror-transformation sheet) +identity-transformation+))

(defmethod note-sheet-degrafted :before ((sheet mirrored-sheet-mixin))
  (port-destroy-mirror (port sheet) sheet))

(defmethod note-sheet-grafted :after ((sheet mirrored-sheet-mixin))
  (setf (%sheet-mirror-region sheet) (sheet-region sheet)))

(defmethod note-sheet-degrafted :after ((sheet mirrored-sheet-mixin))
  (setf (%sheet-mirror-region sheet) nil)
  (setf (%sheet-mirror-transformation sheet) +identity-transformation+))

(defmethod sheet-direct-mirror ((sheet sheet-mixin))
  nil)

(defmethod sheet-mirrored-ancestor ((sheet sheet-mixin))
  (let ((parent (sheet-parent sheet)))
    (if (null parent)
	nil
	(sheet-mirrored-ancestor parent))))

(defmethod sheet-mirrored-ancestor ((sheet mirrored-sheet-mixin))
  sheet)

(defmethod sheet-mirror ((sheet sheet-mixin))
  (let ((mirrored-ancestor (sheet-mirrored-ancestor sheet)))
    (if (null mirrored-ancestor)
	nil
	(sheet-direct-mirror mirrored-ancestor))))

(defmethod sheet-mirror ((sheet mirrored-sheet-mixin))
  (sheet-direct-mirror sheet))

(defmethod sheet-native-region ((sheet mirrored-sheet-mixin))
  (let ((this-region (transform-region (sheet-native-transformation sheet)
                                       ;; mirror?!?
        			       (sheet-region sheet))))
    (aif (sheet-parent sheet)
         (region-intersection this-region
        		      (transform-region
        		       (invert-transformation
        			(%sheet-mirror-transformation sheet))
        		       (sheet-native-region it)))
         this-region)))

(defmethod sheet-native-transformation ((sheet mirrored-sheet-mixin))
  (if-let ((parent (sheet-parent sheet)))
    (compose-transformations
     (sheet-native-transformation parent)
     (invert-transformation
      (%sheet-mirror-transformation sheet)))
    (invert-transformation
     (%sheet-mirror-transformation sheet))))

(defmethod (setf sheet-pointer-cursor) :after (cursor (sheet sheet-mixin))
  ;; why??
  (unless (sheet-direct-mirror sheet)
    (let ((msheet (sheet-mirrored-ancestor sheet)))
      (port-set-sheet-pointer-cursor (port msheet) msheet (sheet-pointer-cursor msheet)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; note: relation

(defmethod note-sheet-enabled ((sheet sheet-mixin))
  (declare (ignore sheet))
  nil)

(defmethod note-sheet-disabled ((sheet sheet-mixin))
  (declare (ignore sheet))
  nil)

(defmethod note-sheet-adopted ((sheet sheet-mixin))
  (declare (ignore sheet))
  nil)

(defmethod note-sheet-disowned ((sheet sheet-mixin))
  (declare (ignore sheet))
  nil)

(defmethod note-children-order-changed ((sheet sheet-mixin) old-order)
  (declare (ignore sheet old-order))
  nil)

(defmethod note-sheet-region-changed ((sheet sheet-mixin) old-region)
  (declare (ignore sheet old-region))
  nil)

(defmethod note-sheet-transformation-changed ((sheet sheet-mixin) old-transformation)
  (declare (ignore sheet old-transformation))
  nil)

(defmethod note-successor-enabled ((sheet sheet-mixin) successor)
  (declare (ignore sheet successor))
  nil)

(defmethod note-successor-enabled :after ((sheet sheet-mixin) successor)
  (awhen (sheet-parent sheet)
    (note-successor-enabled it successor)))

(defmethod note-successor-disabled ((sheet sheet-mixin) successor)
  (declare (ignore sheet successor))
  nil)

(defmethod note-successor-disabled :after ((sheet sheet-mixin) successor)
  (awhen (sheet-parent sheet)
    (note-successor-disabled it successor)))

(defmethod note-successor-adopted ((sheet sheet-mixin) successor)
  (declare (ignore sheet successor))
  nil)

(defmethod note-successor-adopted :after ((sheet sheet-mixin) successor)
  (awhen (sheet-parent sheet)
    (note-successor-adopted it successor)))

(defmethod note-successor-disowned ((sheet sheet-mixin) successor)
  (declare (ignore sheet successor))
  nil)

(defmethod note-successor-disowned :after ((sheet sheet-mixin) successor)
  (awhen (sheet-parent sheet)
    (note-successor-adopted it successor)))

(defmethod note-successor-order-changed ((sheet sheet-mixin) successor old-order)
  (declare (ignore sheet successor old-order))
  nil)

(defmethod note-successor-order-changed :after ((sheet sheet-mixin) successor old-order)
  (awhen (sheet-parent sheet)
    (note-successor-order-changed it successor old-order)))

(defmethod note-successor-region-changed ((sheet sheet-mixin) successor old-region)
  (declare (ignore sheet successor old-region))
  nil)

(defmethod note-successor-region-changed :after ((sheet sheet-mixin) successor old-region)
  (awhen (sheet-parent sheet)
    (note-successor-region-changed it successor old-region)))

(defmethod note-successor-transformation-changed ((sheet sheet-mixin) successor old-transformation)
  (declare (ignore sheet successor old-transformation))
  nil)

(defmethod note-successor-transformation-changed :after ((sheet sheet-mixin) successor old-transformation)
  (awhen (sheet-parent sheet)
    (note-successor-transformation-changed it successor old-transformation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; dirty region
(defun add-dirty-region (sheet successor region)
  (let ((dr (region-intersection (sheet-native-region successor)
                                 (transform-region (sheet-native-transformation successor)
                                                   region))))
    (setf (sheet-dirty-region sheet)
          (region-union
           (sheet-dirty-region sheet)
           dr))
    (when (sheet-grafted-p sheet)
      (repaint-sheet sheet dr))
    (with-bounding-rectangle* (x1 y1 x2 y2)
        dr
        #+nil (log:info "dirty: ~A" (mapcar #'round (list x1 y1 x2 y2))))))

(defmethod note-sheet-enabled :after ((sheet sheet-dirty-region-mixin))
  (add-dirty-region sheet sheet (sheet-region sheet)))
  
(defmethod note-sheet-disabled :after ((sheet sheet-dirty-region-mixin))
  (add-dirty-region sheet sheet (sheet-region sheet)))

(defmethod note-sheet-adopted :after ((sheet sheet-dirty-region-mixin))
  (add-dirty-region sheet sheet (sheet-region sheet)))

(defmethod note-sheet-disowned :after ((sheet sheet-dirty-region-mixin))
  (add-dirty-region sheet sheet (sheet-region sheet)))

(defmethod note-children-order-changed :after ((sheet sheet-dirty-region-mixin) old-order)
  (add-dirty-region sheet sheet (sheet-region sheet)))

(defmethod note-sheet-region-changed :after ((sheet sheet-dirty-region-mixin) old-region)
  (add-dirty-region sheet sheet (region-union 
                                 (sheet-region sheet)
                                 old-region)))

(defmethod note-sheet-transformation-changed :after ((sheet sheet-dirty-region-mixin) old-transformation)
  (add-dirty-region sheet sheet (sheet-region sheet)))

(defmethod note-successor-enabled :after ((sheet sheet-dirty-region-mixin) successor)
  (add-dirty-region sheet successor (sheet-region successor)))
  
(defmethod note-successor-disabled :after ((sheet sheet-dirty-region-mixin) successor)
  (add-dirty-region sheet successor (sheet-region successor)))

(defmethod note-successor-adopted :after ((sheet sheet-dirty-region-mixin) successor)
  (add-dirty-region sheet successor (sheet-region successor)))

(defmethod note-successor-disowned :after ((sheet sheet-dirty-region-mixin) successor)
  (add-dirty-region sheet successor (sheet-region successor)))

(defmethod note-successor-order-changed :after ((sheet sheet-dirty-region-mixin) successor old-order)
  (add-dirty-region sheet successor (sheet-region)))

(defmethod note-successor-region-changed :after ((sheet sheet-dirty-region-mixin) successor old-region)
  (add-dirty-region sheet successor (region-union 
                                     (sheet-region successor)
                                     old-region)))

(defmethod note-successor-transformation-changed :after ((sheet sheet-dirty-region-mixin) successor old-transformation)
  (add-dirty-region sheet successor (sheet-region successor)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; graft

(defmethod graft ((sheet sheet-mixin))
  nil)

(defmethod graft ((sheet sheet-graft-mixin))
  sheet)

(defmethod graft ((sheet sheet-parent-mixin))
  (and (sheet-parent sheet) (graft (sheet-parent sheet))))

(defmethod sheet-grafted-p ((sheet sheet-mixin))
  nil)

(defmethod sheet-grafted-p ((sheet sheet-parent-mixin))
  (if (sheet-parent sheet)
      (sheet-grafted-p (sheet-parent sheet))
      nil))

(defmethod sheet-grafted-p ((graft sheet-graft-mixin))
  (declare (ignore graft))
  t)

(defmethod note-sheet-grafted ((sheet sheet-mixin))
  (mapc #'note-sheet-grafted (sheet-children sheet)))

(defmethod note-sheet-degrafted ((sheet sheet-mixin))
  (mapc #'note-sheet-degrafted (sheet-children sheet)))

