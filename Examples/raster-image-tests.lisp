(in-package :clim-demo)

(defparameter *raster-image-tests* (make-hash-table :test 'equal))

(defparameter *raster-image-width* 500)
(defparameter *raster-image-height* 700)
(defparameter *raster-image-border-width* 5)

(defstruct raster-image-test name description drawer)

(defmacro define-raster-image-test (name arglist description &body body)
  (check-type name string)
  (check-type description string)
  `(setf (gethash ,name *raster-image-tests*)
         (make-raster-image-test :name ,name
                                 :description ,description
                                 :drawer (lambda ,arglist ,@body))))

(define-application-frame raster-image-tests ()
  ((signal-condition-p :initform nil)
   (current-selection :initform nil))
  (:panes
   (output :application-pane
           :min-width *raster-image-width*
           :min-height *raster-image-height*
           :display-time nil
           :display-function #'raster-image-display-output
           :end-of-line-action :wrap
           :end-of-page-action :wrap)
   (description :application-pane)
   (selector :list-pane
             :mode :exclusive
             :name-key #'raster-image-test-name
             :items (sort (loop for x being the hash-values of *raster-image-tests*
                                collect x) #'string< :key #'raster-image-test-name)
             :value-changed-callback #'raster-image-update-selection)
   (condition-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback 'raster-image-update-condition-option)
      (clim:radio-box-current-selection "message")
      "break")))
  (:layouts
   (default
       (spacing (:thickness 3)
         (horizontally ()
           (vertically ()
             (spacing (:thickness 3)
               (clim-extensions:lowering ()
                 (scrolling (:scroll-bar :vertical :height *raster-image-height*) selector)))
             (labelling (:label "Condition")
               condition-option))
           (vertically ()
             (spacing (:thickness 3)
               (clim-extensions:lowering ()
                 (horizontally ()
                   (labelling (:label "Backend")
                     (climi::bordering (:border-width *raster-image-border-width*)
                       output)))))
             (spacing (:thickness 3)
               (clim-extensions:lowering ()
                 (scrolling (:scroll-bar :vertical :height 200) description)))))))))

(defun raster-image-update-condition-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (signal-condition-p) clim:*application-frame*
    (setf signal-condition-p
	  (string= (clim:gadget-label selected-gadget) "break"))))

(defun raster-image-update-selection (pane item)
  (declare (ignore pane))
  (with-slots (current-selection) clim:*application-frame*
    (setf current-selection item))
  (window-clear (get-frame-pane *application-frame* 'description))
  (redisplay-frame-pane *application-frame* (get-frame-pane *application-frame* 'output) :force-p t))

(defun raster-image-display-output (frame pane)
  (declare (ignore pane))
  (let ((output (get-frame-pane frame 'output))
        (item (slot-value frame 'current-selection)))
    (let ((description (get-frame-pane *application-frame* 'description)))
      (when item
        (with-text-style (description (make-text-style :sans-serif :roman :normal))
          (write-string (raster-image-test-description item) description)
          (terpri description))
        (if (slot-value *application-frame* 'signal-condition-p)
            (clim:with-drawing-options (output :clipping-region
                                               (clim:make-rectangle* 0 0 *raster-image-width* *raster-image-height*))
              (clim:draw-rectangle* output 0 0 *width* *height* :filled t
                                    :ink clim:+grey90+)
              (funcall (raster-image-test-drawer item) output))
            (handler-case
                (clim:with-drawing-options (output :clipping-region
                                                   (clim:make-rectangle* 0 0 *width* *height*))
                  (clim:draw-rectangle* output 0 0 *width* *height* :filled t
                                        :ink clim:+grey90+)
                  (funcall (raster-image-test-drawer item) output))
              (condition (condition)
                (clim:with-drawing-options (description :ink +red+)
                  (format description "Backend:~a~%" condition)))))))))


(defun run-raster-image-tests ()
  (run-frame-top-level
   (make-application-frame
    'raster-image-tests)))

(define-raster-image-test "01) Colors" (stream)
    "Conversion berween real values and octets"
  (clim:formatting-table (stream :x-spacing 50
				 :y-spacing 20)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream)
        (format stream "Octets -> Values"))
      (clim:formatting-cell (stream)
        (let ((v '(0 26 76 128 178 230 255)))
          (format stream "~A ->~%~A" v
                  (mapcar #'clim-image:color-octet->value v)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream)
        (format stream "Values -> Octets"))
      (clim:formatting-cell (stream)
        (let ((v '(0  0.3 0.5 0.7 0.9 1)))
          (format stream "~A ->~%~A" v
                  (mapcar #'clim-image:color-value->octet v)))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream)
        (format stream "Blending"))
      (clim:formatting-cell (stream)
        (let* ((v1 '(1.0 0.0 0.0 1.0))
               (v2 '(0.0 1.0 0.0 1.0))
               (v (append v1 v2)))
          (format stream "F:~A | B:~A ->~%~A~%F:~A | B:~A ->~%~A"
                  v1 v2
                  (multiple-value-list (apply #'climi::color-blend-function v))
                  (mapcar #'clim-image:color-value->octet v1)
                  (mapcar #'clim-image:color-value->octet v2)
                  (multiple-value-list (apply #'clim-image:octet-blend-function
                                              (mapcar #'clim-image:color-value->octet (append v2 v1))))))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream)
        (format stream "Blending"))
      (clim:formatting-cell (stream)
        (let* ((v1 '(1.0 0.0 0.0 0.5))
               (v2 '(0.0 1.0 0.0 1.0))
               (v (append v1 v2)))
          (format stream "F:~A | B:~A ->~%~A~%F:~A | B:~A ->~%~A"
                  v1 v2
                  (multiple-value-list (apply #'climi::color-blend-function v))
                  (mapcar #'clim-image:color-value->octet v1)
                  (mapcar #'clim-image:color-value->octet v2)
                  (multiple-value-list (apply #'clim-image:octet-blend-function
                                              (mapcar #'clim-image:color-value->octet (append v2 v1))))))))))

(define-raster-image-test "02) simple -2da " (stream)
    "Simple drawing of two dimensional array of pixels: white, purple, olive."
  (let* ((image (clim-image:make-rgb-image 90 70))
         (pixels (clim-image:image-pixels image)))
    (clim-image:draw-image* stream image 10 10)
    (dotimes (x 90)
      (dotimes (y 70)
        (setf (aref pixels y x) #x800080)))
    (clim-image:draw-image* stream image 120 10)
    (dotimes (x 90)
      (dotimes (y 70)
        (setf (aref pixels y x) #x808000)))
    (with-translation (stream 110 0)
      (clim-image:draw-image* stream image 120 10))))

(define-raster-image-test "02) simple - opt" (stream)
    "Simple drawing of opticl pixels: white, purple, olive."
  (let* ((image (clim-image:make-opticl-rgb-image 90 70))
         (pixels (clim-image:image-pixels image)))
    (clim-image:draw-image* stream image 10 10)
    (dotimes (x 90)
      (dotimes (y 70)
        (setf (opticl:pixel pixels y x) (values #x80 #x00 #x80))))
    (clim-image:draw-image* stream image 120 10)
    (dotimes (x 90)
      (dotimes (y 70)
        (setf (opticl:pixel pixels y x) (values #x80 #x80 #x00))))
    (with-translation (stream 110 0)
      (clim-image:draw-image* stream image 120 10))))


(defparameter *opticl-testing-image-directory* (uiop/pathname:merge-pathnames* "test/images/" (asdf:system-source-directory (asdf:find-system :opticl))))
(defparameter *opticl-testing-image-files* '("camel-indexed.tiff"
                                             "truck-gray.jpeg"
                                             "truck-gray-none.tiff"
                                             "truck-rgb-deflate.tiff"
                                             "truck-rgb-none.tiff"
                                             "truck-small.png"
                                             "horse-16-bit.tiff"
                                             "truck-gray-jpeg.tiff"
                                             "truck-gray-packbits.tiff"
                                             "truck-rgb-jpeg.tiff"
                                             "truck-rgb-packbits.tiff"
                                             "truck-gray-deflate.tiff"
                                             "truck-gray-lzw.tiff"
                                             "truck.jpeg"
                                             "truck-rgb-lzw.tiff"
                                             "truck-small.jpeg"))

(define-raster-image-test "03) read " (stream)
    "Simple loading..."
  (let ((h 10)
        (w 10))
    (dolist (file *opticl-testing-image-files*)
      (handler-case
          (let ((path
                 (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*)))
            (let ((image (clim-image:read-image path)))
              (clim-image:draw-image* stream image 10 h)
              (setf h (+ h (clim-image:image-height image) 10))))
        (condition (condition)
          (clim:with-drawing-options (stream :ink +red+)
            (clim:draw-text* stream "Error" 20 h)
              (setf h (+ h 30))))))))

(define-raster-image-test "03) read -coercion " (stream)
    "Simple loading..."
  (let ((h 10)
        (w 10))
    (dolist (file *opticl-testing-image-files*)
      (handler-case
          (let ((path
                 (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*)))
            (let ((image (clim-image:read-image path)))
              (clim-image:draw-image* stream image 10 h)
              (setf h (+ h (clim-image:image-height image) 10))
              (let ((2da-image (clim-image:coerce-image image 'clim-image:rgb-image)))
                (clim-image:draw-image* stream 2da-image 200 (- h (clim-image:image-height image) 10)))))
        (condition (condition)
          (clim:with-drawing-options (stream :ink +red+)
            (clim:draw-text* stream "Error" 20 h)
              (setf h (+ h 30))))))))
