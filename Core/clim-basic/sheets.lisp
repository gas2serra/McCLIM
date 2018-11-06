;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 1998,1999,2000 by Michael McDonald (mikemac@mikemac.com),
;;;  (c) copyright 2000 by
;;;           Iban Hatchondo (hatchond@emi.u-bordeaux.fr)
;;;           Julien Boninfante (boninfan@emi.u-bordeaux.fr)
;;;  (c) copyright 2000, 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2001 by
;;;           Arnaud Rouanet (rouanet@emi.u-bordeaux.fr)
;;;           Lionel Salabartan (salabart@emi.u-bordeaux.fr)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The sheet protocol

(in-package :clim-internals)

(defgeneric raise-sheet-internal (sheet parent))
(defgeneric bury-sheet-internal (sheet parent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; input protocol

(defgeneric dispatch-event (client event))
(defgeneric queue-event (client event))
(defgeneric schedule-event (client event delay))
(defgeneric handle-event (client event))
(defgeneric event-read (client))
(defgeneric event-read-no-hang (client))
(defgeneric event-peek (client &optional evet-type))
(defgeneric event-unread (client event))
(defgeneric event-listen (client))

;;; These DEFGENERIC forms are commented out because they appear
;;; in decls.lisp.
;(defgeneric sheet-direct-mirror (sheet))
;(defgeneric sheet-mirrored-ancestor (sheet))
;(defgeneric sheet-mirror (sheet))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; repaint protocol

(defgeneric dispatch-repaint (sheet region))
;(defgeneric queue-repaint (sheet region))
;(defgeneric handle-repaint (sheet region))
;(defgeneric repaint-sheet (sheet region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; notification protocol

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; non standard protocol



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; sheet protocol class

(defclass basic-sheet (sheet-mixin sheet-cached-mixin sheet)
  ((region :type region
	   :initarg :region
	   :initform (make-bounding-rectangle 0 0 100 100)
	   :accessor sheet-region
           :writer %%set-sheet-region)
   #+nil (native-transformation :type (or null transformation)
			  :initform nil
                          ;; :writer %%set-sheet-native-transformation
                          ;; :reader %%sheet-native-transformation
                          )
   #+nil (native-region :type (or null region)
                  :initarg :native-region
		  :initform nil)
   #+nil (device-transformation :type (or null transformation)
                          :initform nil)
   #+nil (device-region :type (or null region)
		  :initform nil)
      ))

;;; Native region is volatile, and is only computed at the first
;;; request when it's equal to nil.
;;;



;;; Instead of defining yet another function we specialize on
;;; sequence. Thanks to that we can map over "all-but-parent" sheets
;;; with `(map-over-sheets function (sheet-children sheet))'.
#|

(defmethod (setf sheet-enabled-p) :after (enabled-p (sheet basic-sheet))
  (dispatch-repaint (sheet-parent sheet)
                    (transform-region (sheet-transformation sheet)
                                      (sheet-region sheet))))

(defmethod (setf sheet-region) :around (region (sheet basic-sheet))
  (unless (region-equal region (sheet-region sheet))
    (let ((old-region (sheet-region sheet)))
      (let ((*inhibit-dispatch-repaint* t))
        (call-next-method))
      (when (sheet-viewable-p sheet)
        (dispatch-repaint (sheet-parent sheet)
                          (transform-region (sheet-transformation sheet)
                                            (region-union (sheet-region sheet)
                                                          old-region)))))))

(defmethod (setf sheet-transformation) :around (transformation (sheet basic-sheet))
  (unless (transformation-equal transformation (sheet-transformation sheet))
    (let ((old-transformation (sheet-transformation sheet)))
      (let ((*inhibit-dispatch-repaint* t))
        (call-next-method))
      (when (sheet-viewable-p sheet)
        (let ((new-region (transform-region (sheet-transformation sheet) (sheet-region sheet)))
              (old-region (transform-region old-transformation (sheet-region sheet))))
          (dispatch-repaint (sheet-parent sheet)
                            (region-union new-region old-region)))))))
|#

(defun %set-sheet-region-and-transformation
    (sheet &optional
             (region (sheet-region sheet) new-region-p)
             (transformation (sheet-transformation sheet) new-transformation-p))
  (unless (or new-region-p new-transformation-p)
    (return-from %set-sheet-region-and-transformation nil))
  (let ((old-transformation (sheet-transformation sheet))
        (old-region (sheet-region sheet)))
    (let ((*inhibit-dispatch-repaint* t))
      (setf (sheet-region sheet) region
            (sheet-transformation sheet) transformation))
    #+nil (when (sheet-viewable-p sheet)
      (let ((new-region (transform-region transformation region))
            (old-region (transform-region old-transformation old-region)))
        (dispatch-repaint (sheet-parent sheet)
                          (region-union new-region old-region))))))


(defmethod move-and-resize-sheet ((sheet basic-sheet) x y width height)
  (let ((transform (sheet-transformation sheet)))
    (multiple-value-bind (old-x old-y)
        (transform-position transform 0 0)
      (%set-sheet-region-and-transformation
       sheet
       (make-bounding-rectangle 0 0 width height)
       (compose-translation-with-transformation transform (- x old-x) (- y old-y))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mirrored sheet

#|
(defmethod sheet-direct-mirror ((sheet mirrored-sheet-mixin))
  (port-lookup-mirror (port sheet) sheet))
|#

(defmethod bounding-rectangle* ((sheet sheet))
  (bounding-rectangle* (sheet-region sheet)))

;;; The null sheet

(defclass null-sheet (basic-sheet) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dangerous codes
;;; postfix: %%% 
;;;

;; used by invoke-with-double-buffering
(defmacro with-temp-mirror%%% ((mirrored-sheet new-mirror new-native-transformation new-region)
			       &body body)
  (let ((ori-native-transformation (gensym "ori-transformation"))
	(ori-region (gensym "ori-region")))
    ;; how use gensym in flet?
    `(flet ((set-native (transform region sheet)
	      (invalidate-cached-regions sheet)
	      (invalidate-cached-transformations sheet)
	      #+nil (%%set-sheet-native-transformation transform sheet)
              (setf (%sheet-mirror-transformation sheet)
                    (invert-transformation transform))
              #+nil(log:info "===> ~A ~A ~A"
                        sheet (sheet-native-transformation sheet)
                        (%sheet-mirror-transformation sheet))

	      (setf (slot-value sheet 'region) region))
            #+nil ((setf sheet-direct-mirror) (new-mirror sheet)
              (port-register-mirror (port sheet) sheet new-mirror)))
       (let ((,ori-native-transformation (sheet-native-transformation ,mirrored-sheet))
	     (,ori-region (sheet-region ,mirrored-sheet)))
	 (letf (((sheet-parent ,mirrored-sheet) nil)
		((sheet-direct-mirror ,mirrored-sheet) ,new-mirror))
	   (unwind-protect
		(progn
		  (set-native ,new-native-transformation ,new-region ,mirrored-sheet)
		  ,@body)
	     (set-native ,ori-native-transformation ,ori-region ,mirrored-sheet)))))))



#|
(defmethod initialize-instance :after ((sheet basic-sheet) &key parent children)
  (when parent
    (sheet-adopt-child parent sheet))
  (dolist (child children)
    (sheet-adopt-child sheet child)))
|#
