(in-package :it.bese.fiveam)

(in-suite* :it.bese.fiveam)

(def-suite sheet-suite :description "Suite for sheet tests.")
(in-suite* sheet-suite)

(defparameter *note-adopted* nil)
(defparameter *note-disowned* nil)
(defparameter *note-enabled* nil)
(defparameter *note-disabled* nil)


(defclass dummy-sheet-mixin ()
  ())

(defmethod clim:note-sheet-adopted :after ((sheet dummy-sheet-mixin))
  (push sheet *note-adopted*)) 

(defmethod clim:note-sheet-disowned :after ((sheet dummy-sheet-mixin))
  (push sheet *note-disowned*))

(defmethod clim:note-sheet-enabled :after ((sheet dummy-sheet-mixin))
  (push sheet *note-enabled*))

(defmethod clim:note-sheet-disabled :after ((sheet dummy-sheet-mixin))
  (push sheet *note-disabled*))

(defclass dummy-sheet (dummy-sheet-mixin clim:basic-sheet)
  ())

(defclass dummy-sheet-parent (dummy-sheet-mixin clim:sheet-parent-mixin
                                                clim:basic-sheet)
  ())

(defclass dummy-sheet-leaf (dummy-sheet-mixin clim:sheet-leaf-mixin
                                              clim:basic-sheet)
  ())

(defclass dummy-sheet-single-child (dummy-sheet-mixin clim:sheet-single-child-mixin
                                                      clim:basic-sheet)
  ())

(defclass dummy-sheet-multiple-child (dummy-sheet-mixin clim:sheet-multiple-child-mixin
                                                        clim:basic-sheet)
  ())

(defclass dummy-root-sheet (dummy-sheet-mixin clim:sheet-identity-transformation-mixin
                                              clim:sheet-multiple-child-mixin
                                              clim:basic-sheet)
  ())

(defclass dummy-middle-sheet (dummy-sheet-mixin clim:sheet-parent-mixin
                                                clim:sheet-translation-transformation-mixin
                                                clim:sheet-multiple-child-mixin
                                                clim:basic-sheet)
  ())

(defclass dummy-leaf-sheet (dummy-sheet-mixin clim:sheet-parent-mixin
                                              clim:sheet-transformation-mixin
                                              clim:sheet-leaf-mixin
                                              clim:basic-sheet)
  ())
;;
;; ToFix: This class is an abstract class, intended only to be subclassed, not instantiated
;;
(def-test base-01 ()
  (let ((sheet (make-instance 'clim:basic-sheet)))
    (is (clim:sheetp sheet))))

(def-test base-02 ()
  (let ((sheet (make-instance 'dummy-sheet)))
    (is (clim:sheetp sheet))))

;;;
;;; structure
;;;

;; simple fixture
(def-fixture sheet-structure (sheet-class)
  (let ((sheet (make-instance sheet-class))
        (parent  (make-instance 'dummy-sheet-multiple-child))
        (parent2 (make-instance 'dummy-sheet-multiple-child))
        (child1 (make-instance 'dummy-sheet-parent))
        (child2 (make-instance 'dummy-sheet-parent))
        (*note-adopted* nil)
        (*note-disowned* nil))
    (declare (ignorable parent parent2 child1 child2))
    `(progn
       ,@(&body))))
;; more complex fixture
(def-fixture sheet-structure-2 ()
  (let ((s  (make-instance 'dummy-root-sheet
                           :region (clim:make-bounding-rectangle 0 0 100 100)
                           ))
        (s.1 (make-instance 'dummy-middle-sheet
                            :region (clim:make-bounding-rectangle 0 0 20 80)
                            :transformation (clim:make-translation-transformation 10 10)))
        (s.2 (make-instance 'dummy-middle-sheet
                            :region (clim:make-bounding-rectangle 0 0 20 80)
                            :transformation (clim:make-translation-transformation 40 10)))
        (s.3 (make-instance 'dummy-middle-sheet
                            :region (clim:make-bounding-rectangle 0 0 40 80)
                            :transformation (clim:make-translation-transformation 55 10)))
        (s.1.1 (make-instance 'dummy-middle-sheet
                              :region (clim:make-bounding-rectangle 0 0 10 20)
                              :transformation (clim:make-translation-transformation 5 10)))
        (s.1.2 (make-instance 'dummy-middle-sheet
                              :region (clim:make-bounding-rectangle 0 0 10 20)
                              :transformation (clim:make-translation-transformation 5 30)))
        (s.1.3 (make-instance 'dummy-middle-sheet
                              :region (clim:make-bounding-rectangle 0 0 10 20)
                              :transformation (clim:make-translation-transformation 5 50)))
        (s.2.1 (make-instance 'dummy-middle-sheet
                              :region (clim:make-bounding-rectangle 0 0 10 20)
                              :transformation (clim:make-translation-transformation 0 0)))
        (s.1.1.1 (make-instance 'dummy-leaf-sheet
                                :region (clim:make-bounding-rectangle 0 0 10 10)
                                :transformation (clim:make-translation-transformation 0 0)))
        (s.1.2.1 (make-instance 'dummy-leaf-sheet
                                :region (clim:make-bounding-rectangle 0 0 10 10)
                                :transformation (clim:make-translation-transformation 0 0)))
        (s.1.3.1 (make-instance 'dummy-leaf-sheet
                                :region (clim:make-bounding-rectangle 0 0 10 10)
                                :transformation (clim:make-translation-transformation 0 0)))
        (s.2.1.1 (make-instance 'dummy-leaf-sheet
                                :region (clim:make-bounding-rectangle 0 0 10 10)
                                :transformation (clim:make-translation-transformation 0 0)))
        (s.3.1 (make-instance 'dummy-leaf-sheet
                              :region (clim:make-bounding-rectangle 0 0 10 20)
                              :transformation (clim:make-translation-transformation 5 5)))
        (*note-adopted* nil)
        (*note-disowned* nil)
        (*note-enabled* nil)
        (*note-disabled* nil))
    (clim:sheet-adopt-child s s.1)
    (clim:sheet-adopt-child s.1 s.1.1)
    (clim:sheet-adopt-child s.1.1 s.1.1.1)
    (clim:sheet-adopt-child s.1 s.1.2)
    (clim:sheet-adopt-child s.1.2 s.1.2.1)
    (clim:sheet-adopt-child s.1 s.1.3)
    (clim:sheet-adopt-child s.1.3 s.1.3.1)
    (clim:sheet-adopt-child s s.2)
    (clim:sheet-adopt-child s.2 s.2.1)
    (clim:sheet-adopt-child s.2.1 s.2.1.1)    
    (clim:sheet-adopt-child s s.3)
    (clim:sheet-adopt-child s.3 s.3.1)
    `(progn
       ,@(&body))))


;; tests
(def-test structure-01 (:fixture (sheet-structure 'dummy-sheet-parent))
  (is (clim:sheetp sheet))
  (is (eq nil (clim:sheet-parent sheet)))
  (is (eq nil (clim:sheet-children sheet)))
  (finishes
    (clim:sheet-adopt-child parent sheet))
  (is (equal *note-adopted* (list sheet)))
  (signals clim:sheet-already-has-parent
    (clim:sheet-adopt-child parent2 sheet))
  (signals error (clim:sheet-adopt-child sheet child1))
  (signals clim:sheet-is-not-child (clim:sheet-disown-child sheet child1))
  (is (equal *note-adopted* (list sheet)))
  (is (equal *note-disowned* nil))
  (finishes (clim:sheet-disown-child parent sheet))
  (signals clim:sheet-is-not-child (clim:sheet-disown-child parent sheet))
  (is (equal *note-disowned* (list sheet))))

(def-test structure-02 (:fixture (sheet-structure 'dummy-sheet-leaf))
  (is (clim:sheetp sheet))
  (is (eq nil (clim:sheet-parent sheet)))
  (is (eq nil (clim:sheet-children sheet)))
  (is (equal nil (clim:sheet-children parent)))
  (signals error
    (clim:sheet-adopt-child parent sheet))
  (is (equal nil (clim:sheet-children parent)))
  (is (equal *note-adopted* nil))
  (signals error (clim:sheet-adopt-child sheet child1))
  (signals clim:sheet-is-not-child (clim:sheet-disown-child parent sheet))
  (is (equal *note-adopted* nil))
  (is (equal *note-disowned* nil)))

(def-test structure-03 (:fixture (sheet-structure 'dummy-sheet-single-child))
  (is (clim:sheetp sheet))
  (is (eq nil (clim:sheet-parent sheet)))
  (is (eq nil (clim:sheet-children sheet)))
  (signals error
    (clim:sheet-adopt-child parent sheet))
  (finishes (clim:sheet-adopt-child sheet child1))
  (is (clim:sheet-children sheet) (list child1))
  (is (equal *note-adopted* (list child1)))
  (signals clim:sheet-supports-only-one-child (clim:sheet-adopt-child sheet child2))
  (is (equal *note-adopted* (list child1)))
  (is (equal *note-disowned* nil))
  (finishes (clim:sheet-disown-child sheet child1))
  (is (equal *note-disowned* (list child1))))

(def-test structure-04 (:fixture (sheet-structure 'dummy-sheet-multiple-child))
  (is (clim:sheetp sheet))
  (is (eq nil (clim:sheet-parent sheet)))
  (is (eq nil (clim:sheet-children sheet)))
  (signals error
    (clim:sheet-adopt-child parent sheet))
  (is (equal *note-adopted* nil))
  (finishes (clim:sheet-adopt-child sheet child1))
  (is (equal *note-adopted* (list child1)))
  (finishes (clim:sheet-adopt-child sheet child2))
  (is (equal *note-adopted* (list child2 child1)))
  (signals clim:sheet-already-has-parent (clim:sheet-adopt-child sheet child1))
  (is (equal *note-adopted* (list child2 child1)))
  (is (equal *note-disowned* nil))
  (finishes (clim:sheet-disown-child sheet child1))
  (finishes (clim:sheet-disown-child sheet child2))
  (is (equal *note-disowned* (list child2 child1))))

;; ancestor reorder-sheets
(def-test structure-05 (:fixture (sheet-structure-2))
  (is (clim:sheet-ancestor-p s.1.2 s.1))
  (is (clim:sheet-ancestor-p s.1.2.1 s))
  (is-false (clim:sheet-ancestor-p s s.1.1.1))
  (is (clim:sheet-ancestor-p s.1 s.1))
  (is-false (clim:sheet-ancestor-p s.1 s.2))
  (is (equal (clim:sheet-siblings s.1) (list s.3 s.2)))
  (is (equal (clim:sheet-siblings s.1.2) (list s.1.3 s.1.1)))
  (is (equal (clim:sheet-children s.1) (list s.1.3 s.1.2 s.1.1)))
  (clim:raise-sheet s.1.2)
  (is (equal (clim:sheet-children s.1) (list s.1.2 s.1.3 s.1.1)))
  (clim:raise-sheet s.1.2)
  (is (equal (clim:sheet-children s.1) (list s.1.2 s.1.3 s.1.1)))
  (clim:raise-sheet s.1.1)
  (is (equal (clim:sheet-children s.1) (list s.1.1 s.1.2 s.1.3)))
  (clim:bury-sheet s.1.1)
  (clim:bury-sheet s.1.3)
  (is (equal (clim:sheet-children s.1) (list s.1.2 s.1.1 s.1.3)))
  (clim:bury-sheet s.1.3)
  (is (equal (clim:sheet-children s.1) (list s.1.2 s.1.1 s.1.3)))
  (clim:bury-sheet s.1.2)
  (is (equal (clim:sheet-children s.1) (list s.1.1 s.1.3 s.1.2)))
  (clim:reorder-sheets s.1 (list s.1.1 s.1.2 s.1.3))
  (is (equal (clim:sheet-children s.1) (list s.1.1 s.1.2 s.1.3)))
  (signals clim:sheet-ordering-underspecified
    (clim:reorder-sheets s.1 (list s.1.1 s.1.3)))
  (is (equal (clim:sheet-children s.1) (list s.1.1 s.1.2 s.1.3)))
  (signals clim:sheet-ordering-underspecified
    (clim:reorder-sheets s.1 (list s.1.1 s.1.3 s.2.1)))
  (signals clim:sheet-is-not-child
    (clim:reorder-sheets s.1 (list s.1.1 s.1.3 s.1.2 s.2))))

;; enabled note-sheet-enabled note-sheet-disabled
(def-test structure-06 (:fixture (sheet-structure-2))
  (let ((sheets))
    (clim:map-over-sheets #'(lambda (x) (push x sheets)) s.2)
    (is (equal sheets (list s.2.1.1 s.2.1 s.2))))
  (is (equal (clim:sheet-enabled-children s.2) (list s.2.1)))
  (is (equal (clim:sheet-enabled-children s.1) (list s.1.3 s.1.2 s.1.1)))
  (setf (clim:sheet-enabled-p s.1) t)
  (is (equal *note-enabled* nil))
  )

(def-test structure-06 (:fixture (sheet-structure-2))
  ;; it call repaint!! it is not grafted!
  (finishes (setf (clim:sheet-enabled-p s.1.2) nil)))

;;  sheet-region sheet-transformation
(def-test structure-07 (:fixture (sheet-structure-2))
  (is (equal s.1 (clim:child-containing-position s 15 20)))
  (is (equal s.2 (clim:child-containing-position s 50 70)))
  (is (equal s.3 (clim:child-containing-position s 75 50)))
  (is (equal nil (clim:child-containing-position s 99 99)))
  (is (clim:region-equal (clim:make-bounding-rectangle 10 10 30 90)
                         (clim:sheet-native-region s.1)))
  (is (clim:region-equal (clim:make-bounding-rectangle 15 20 25 40)
                         (clim:sheet-native-region s.1.1)))

  (is (list -5 -10)
      (multiple-value-list (clim:map-sheet-position-to-child s.1.1 0 0)))
  (is (list 5 10)
      (multiple-value-list (clim:map-sheet-position-to-parent s.1.1 0 0)))
  (is (equal
       (list -5 -10 0 0)
       (multiple-value-list (clim:map-sheet-rectangle*-to-child s.1.1 0 0 5 10))))
  (is (equal
       (list 5 10 10 20)
       (multiple-value-list (clim:map-sheet-rectangle*-to-parent s.1.1 0 0 5 10))))
    
  (is (clim:transformation-equal
       (clim:sheet-delta-transformation s.1 s)
       (clim:make-translation-transformation 10 10)))
  (is (clim:transformation-equal
       (clim:sheet-delta-transformation s.1.1 s)
       (clim:make-translation-transformation 15 20)))
  (signals clim:sheet-is-not-ancestor (clim:sheet-delta-transformation s.1.1 s.2))
  (let ((sheets nil))
    (clim:map-over-sheets-containing-position #'(lambda (s)
                                                  (push s sheets))
                                              s 20 30)
    (is (equal sheets (list s.1))))
  ;; ToFix different order
  (let ((sheets nil))
    (clim:map-over-sheets-overlapping-region #'(lambda (s)
                                                 (push s sheets))
                                             s
                                             (clim:make-bounding-rectangle 10 10 50 50))
    (is (equal sheets (list s.1 s.2))))
  (let ((sheets
         (clim:children-overlapping-region s
                                           (clim:make-bounding-rectangle 10 10 50 50))))
    (is (equal sheets (list s.2 s.1))))
  (let ((sheets
         (clim:children-overlapping-rectangle* s
                                               10 10 50 50)))
    (is (equal sheets (list s.2 s.1))))
  ;; error!!
  (let ((sheets
         (clim:sheet-occluding-sheets s s.1)))
    (is (equal (list) sheets)))
  (let ((sheets
         (clim:sheet-occluding-sheets s s.2)))
    (is (equal (list s.3) sheets)))
  (let ((sheets
         (clim:sheet-occluding-sheets s s.3)))
    (is (equal (list ) sheets)))

  (is (clim:region-equal (clim:make-bounding-rectangle 10 10 30 90)
                         (clim:sheet-allocated-region s s.1)))
  (is (clim:region-equal (clim:make-bounding-rectangle 40 10 55 90)
                         (clim:sheet-allocated-region s s.2)))
  (is (clim:region-equal (clim:make-bounding-rectangle 55 10 95 90)
                         (clim:sheet-allocated-region s s.3)))
  )



;; move-sheet
;; resize-sheet move-and-resize-sheet

(def-test structure-08 (:fixture (sheet-structure-2))
  (is (clim:region-equal (clim:make-bounding-rectangle 0 0 10 20)
                         (clim:sheet-region s.1.1)))
  (is (clim:region-equal (clim:make-bounding-rectangle 15 20 25 40)
                         (clim:sheet-native-region s.1.1)))
  (is (list 5 10)
      (multiple-value-list (clim:map-sheet-position-to-parent s.1.1 0 0)))
  (clim:move-sheet s.1.1 10 20)
  (is (list 10 20)
      (multiple-value-list (clim:map-sheet-position-to-parent s.1.1 0 0)))
  (is (clim:region-equal (clim:make-bounding-rectangle 20 30 30 50)
                         (clim:sheet-native-region s.1.1)))
  (clim:resize-sheet s.1.1 5 5)
  (is (clim:region-equal (clim:make-bounding-rectangle 0 0 5 5)
                         (clim:sheet-region s.1.1)))
  ;; intersect with the parents... (visible!??!)
  (is-false (clim:region-equal (clim:make-bounding-rectangle 20 30 30 50)
                               (clim:sheet-native-region s.1.1)))
  (is (clim:region-equal (clim:make-bounding-rectangle 20 30 25 35)
                         (clim:sheet-native-region s.1.1)))
  
  )
     
;; sheet-direct-mirror sheet-mirror
;; 

;; test grafted/degrafted sheet-viewable-p note-sheet-grafted note-sheet-degrafted
