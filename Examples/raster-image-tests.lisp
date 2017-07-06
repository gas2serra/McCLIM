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
                   (labelling (:label "Stream")
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
                  (format description "Error:~a~%" condition)))))))))


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

(define-raster-image-test "02) simple - 2 dim array " (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
  (let* ((image (clim-image:make-rgb-image 90 70))
         (pixels (clim-image:image-pixels image)))
    (flet ((draw-rect (color w h)
             (dotimes (x 90)
               (dotimes (y 70)
                 (setf (aref pixels y x) color)))
             (clim-image:draw-image* stream image w h)))
      (draw-rect #xFFFFFF 10 10)
      (draw-rect #x000000 110 10)
      (draw-rect #x0000F0 10 100)
      (draw-rect #x00F000 110 100)
      (draw-rect #xF00000 210 100)
      (draw-rect #x800080 10 200)
      (draw-rect #x808000 110 200))))

(define-raster-image-test "02) simple - opticl" (stream)
    "Simple drawing of opticl image: white, black;
red, green, blue;
purple, olive."
  (let* ((image (clim-image:make-opticl-rgb-image 90 70))
         (pixels (clim-image:image-pixels image)))
    (flet ((draw-rect (color w h)
             (let ((b (first color))
                   (g (second color))
                   (r (third color)))
               (dotimes (x 90)
                 (dotimes (y 70)
                   (setf (opticl:pixel pixels y x) (values r g b)))))
             (clim-image:draw-image* stream image w h)))
      (draw-rect (list #xFF #xFF #xFF) 10 10)
      (draw-rect (list #x00 #x00 #x00) 110 10)
      (draw-rect (list #x00 #x00 #xF0) 10 100)
      (draw-rect (list #x00 #xF0 #x00) 110 100)
      (draw-rect (list #xF0 #x00 #x00) 210 100)
      (draw-rect (list #x80 #x00 #x80) 10 200)
      (draw-rect (list #x80 #x80 #x00) 110 200))))

(defparameter *opticl-testing-image-directory* (uiop/pathname:merge-pathnames* "Examples/images/" (asdf:system-source-directory (asdf:find-system :mcclim))))
(defparameter *opticl-testing-image-files* '("pippo-rgb.png"
                                             "pippo-rgba.png"))


(define-raster-image-test "03) read " (stream)
    "Simple loading."
  (let ((h 10)
        (w 10))
    (dolist (file *opticl-testing-image-files*)
      (unwind-protect
           (let ((path
                  (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*)))
             (let ((image (clim-image:read-image path)))
               (clim-image:draw-image* stream image 10 h)
               (setf h (+ h (clim-image:image-height image)))))
        (setf h (+ h 10))))))

(define-raster-image-test "03) read - coercion " (stream)
    "Simple loading and coercion."
  (let ((h 10)
        (w 10))
    (dolist (file *opticl-testing-image-files*)
      (let ((path
             (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*)))
        (let* ((image (clim-image:read-image path))
               (2da-rgba-image (clim-image:coerce-image image 'clim-image:rgba-image))
               (rgba-image (clim-image:coerce-image 2da-rgba-image 'clim-image:opticl-rgba-image)))
          (clim-image:draw-image* stream 2da-rgba-image 10 h)
          (setf h (+ h (clim-image:image-height image) 10))
          (clim-image:draw-image* stream rgba-image 250 (- h (clim-image:image-height image) 10)))))))

(define-raster-image-test "04) image - translation" (stream)
    "draw-design"
  (let ((h 10)
        (w 10))
    (dolist (file *opticl-testing-image-files*)
      (unwind-protect
           (let ((path
                  (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*)))
             (let ((image (clim-image:read-image path)))
                (with-translation (stream 10 h)
                  (clim-image:draw-image* stream image 0 0))
                (setf h (+ h (clim-image:image-height image)))))
        (setf h (+ h 10))))
    (setf h 10)
    (dolist (file *opticl-testing-image-files*)
      (unwind-protect
           (let ((path
                  (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*)))
             (let ((image (clim-image:read-image path)))
               (clim-image:draw-image* stream image 0 0 :transformation (clim:make-translation-transformation 250 h))
               (setf h (+ h (clim-image:image-height image)))))
        (setf h (+ h 10))))))

(define-raster-image-test "04) image - clipping" (stream)
    "clipping"
  (let ((h 10)
        (w 10))
    (dolist (file *opticl-testing-image-files*)
      (unwind-protect
           (let ((path
                  (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*)))
             (let ((image (clim-image:read-image path)))
               (clim-image:draw-image* stream image 10 h :clipping-region (make-rectangle* 10 10 100 100))
               (setf h (+ h (clim-image:image-height image)))))
        (setf h (+ h 10))))))


(define-raster-image-test "05) image design - drawing" (stream)
    "draw-design"
  (let ((h 10)
        (w 10))
    (dolist (file *opticl-testing-image-files*)
      (unwind-protect
           (let ((path
                  (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*)))
             (let ((image (clim-image:read-image path)))
               (clim::draw-design stream (clim-image:make-image-design image) :x 10 :y h)
               (setf h (+ h (clim-image:image-height image)))))
        (setf h (+ h 10))))))

(define-raster-image-test "05) image design - translation" (stream)
    "draw-design"
  (let ((h 10)
        (w 10))
    (dolist (file *opticl-testing-image-files*)
      (unwind-protect
           (let ((path
                  (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*)))
             (let ((image (clim-image:read-image path)))
               (with-translation (stream 10 h)
                 (clim::draw-design stream (clim-image:make-image-design image))
                 (setf h (+ h (clim-image:image-height image)))))
             (setf h (+ h 10)))))
    (setf h 10)
    (dolist (file *opticl-testing-image-files*)
      (unwind-protect
           (let ((path
                  (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*)))
             (let ((image (clim-image:read-image path)))
               (clim::draw-design stream (clim-image:make-image-design image)
                                  :transformation (clim:make-translation-transformation 250 h))
               (setf h (+ h (clim-image:image-height image)))))
        (setf h (+ h 10))))))

(define-raster-image-test "05) image design - clipping" (stream)
    "design clipping"
  (let ((h 10)
        (w 10))
    (dolist (file *opticl-testing-image-files*)
      (unwind-protect
           (let ((path
                  (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*)))
             (let ((image (clim-image:read-image path)))
               (with-translation (stream 10 h)
                 (clim::draw-design stream (clim-image:make-image-design image)
                                    :clipping-region (make-rectangle* 10 10 100 100))
                 (setf h (+ h (clim-image:image-height image)))))
             (setf h (+ h 10)))))
    (setf h 10)
    (dolist (file *opticl-testing-image-files*)
      (unwind-protect
           (let ((path
                  (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*)))
             (let ((image (clim-image:read-image path)))
               (clim::draw-design stream (clim-image:make-image-design image)
                                  :clipping-region (make-rectangle* 10 10 100 100)
                                  :transformation (clim:make-translation-transformation 250 h))
               (setf h (+ h (clim-image:image-height image)))))
        (setf h (+ h 10))))))

(define-raster-image-test "06) generic set fn" (stream)
    "Simple drawing of image: white, black;
red, green, blue;
purple, olive."
  (let* ((image (clim-image:make-opticl-rgb-image 90 70))
         (fn (clim-image:image-rgb-set-fn image)))
    (flet ((draw-rect (fn color w h)
             (multiple-value-bind (r g b)
                 (clim-image:color->octets color)
               (dotimes (x 90)
                 (dotimes (y 70)
                   (funcall fn x y r g b))))
             (clim-image:draw-image* stream image w h)))
      (draw-rect fn +white+ 10 10)
      (draw-rect fn +black+ 110 10)
      (draw-rect fn +red+ 10 100)
      (draw-rect fn +green+ 110 100)
      (draw-rect fn +blue+ 210 100)
      (draw-rect fn +purple+ 10 200)
      (draw-rect fn +olive-drab+ 110 200))))

(define-raster-image-test "07) output record - :draw nil" (stream)
    "output record - no image"
  (let* ((h 10)
         (w 10)
         (file (car *opticl-testing-image-files*))
         (path
          (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*))
         (image (clim-image:read-image path))
         (record
          (clim:with-output-to-output-record (stream)
            (with-output-recording-options (stream :record t :draw nil)
              (clim-image:draw-image* stream image 10 10)
              (draw-rectangle* stream 10 10 50 50 :ink +green+ :filled t)))))))

(define-raster-image-test "07) output record - moving" (stream)
    "output record - two image"
  (let* ((h 10)
         (w 10)
         (file (car *opticl-testing-image-files*))
         (path
          (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*))
         (image (clim-image:read-image path))
         (record
          (clim:with-output-to-output-record (stream)
            (with-output-recording-options (stream :record t :draw t)
              (clim-image:draw-image* stream image 10 10)
              (draw-rectangle* stream 10 10 50 50 :ink +green+ :filled t)))))
    (setf (clim:output-record-position record) (values 10 300))
    (replay record stream)))

(define-raster-image-test "07) output record - design - :draw nil" (stream)
    "output record - design - no image"
  (let* ((h 10)
         (w 10)
         (file (car *opticl-testing-image-files*))
         (path
          (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*))
         (image (clim-image:read-image path))
         (record
          (clim:with-output-to-output-record (stream)
            (with-output-recording-options (stream :record t :draw nil)
              (clim::draw-design stream (clim-image:make-image-design image) :x 10 :y 10)
              (draw-rectangle* stream 10 10 50 50 :ink +green+ :filled t)))))))

(define-raster-image-test "07) output record - design - moving" (stream)
    "output record - design - two image"
  (let* ((h 10)
         (w 10)
         (file (car *opticl-testing-image-files*))
         (path
          (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*))
         (image (clim-image:read-image path))
         (record
          (clim:with-output-to-output-record (stream)
            (with-output-recording-options (stream :record t :draw t)
              (clim::draw-design stream (clim-image:make-image-design image) :x 10 :y 10)
              (draw-rectangle* stream 10 10 50 50 :ink +green+ :filled t)))))
    (setf (clim:output-record-position record) (values 10 300))
    (replay record stream)))

(define-raster-image-test "08) gray - opticl - simple " (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
   (let* ((image (clim-image:make-opticl-gray-image 90 70))
         (pixels (clim-image:image-pixels image)))
    (flet ((draw-rect (color w h)
             (let ((b (first color))
                   (g (second color))
                   (r (third color)))
               (dotimes (x 90)
                 (dotimes (y 70)
                   (setf (opticl:pixel pixels y x) (floor (+ r g b) 3)))))
             (clim-image:draw-image* stream image w h)))
      (draw-rect (list #xFF #xFF #xFF) 10 10)
      (draw-rect (list #x00 #x00 #x00) 110 10)
      (draw-rect (list #x00 #x00 #xF0) 10 100)
      (draw-rect (list #x00 #xF0 #x00) 110 100)
      (draw-rect (list #xF0 #x00 #x00) 210 100)
      (draw-rect (list #x80 #x00 #x80) 10 200)
      (draw-rect (list #x80 #x80 #x00) 110 200))))

(define-raster-image-test "08) gray - opticl - coercion " (stream)
    "Simple loading and coercion."
  (let ((h 10)
        (w 10))
    (dolist (file *opticl-testing-image-files*)
      ;;(handler-case
          (let ((path
                 (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*)))
            (let ((image (clim-image:read-image path)))
              (clim-image:draw-image* stream image 10 h)
              (setf h (+ h (clim-image:image-height image) 10))
              (let ((2da-image (clim-image:coerce-image image 'clim-image:opticl-gray-image)))
                (clim-image:draw-image* stream 2da-image 250 (- h (clim-image:image-height image) 10)))))
      ;;  (condition (condition)
      ;;    (clim:with-drawing-options (stream :ink +red+)
       ;;     (clim:draw-text* stream "Error" 20 h)
          ;;     (setf h (+ h 30))))))))
          )))

(define-raster-image-test "08) gray - 2 dim arr - coercion " (stream)
    "Simple loading and coercion."
  (let ((h 10)
        (w 10))
    (dolist (file *opticl-testing-image-files*)
      ;;(handler-case
          (let ((path
                 (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*)))
            (let ((image (clim-image:read-image path)))
              (clim-image:draw-image* stream image 10 h)
              (setf h (+ h (clim-image:image-height image) 10))
              (let ((2da-image (clim-image:coerce-image image 'clim-image:gray-image)))
                (clim-image:draw-image* stream 2da-image 250 (- h (clim-image:image-height image) 10)))))
      ;;  (condition (condition)
      ;;    (clim:with-drawing-options (stream :ink +red+)
       ;;     (clim:draw-text* stream "Error" 20 h)
          ;;     (setf h (+ h 30))))))))
          )))

(define-raster-image-test "09) stencil - opticl - coercion " (stream)
    "Simple loading and coercion."
  (let ((h 10)
        (w 10))
    (dolist (file *opticl-testing-image-files*)
      ;;(handler-case
          (let ((path
                 (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*)))
            (let ((image (clim-image:read-image path)))
              (clim-image:draw-image* stream image 10 h)
              (setf h (+ h (clim-image:image-height image) 10))
              (let* ((stencil-image (clim-image:coerce-image image 'clim-image:opticl-stencil-image))
                     (gray-image (clim-image:coerce-image stencil-image 'clim-image:opticl-gray-image)))
                (clim-image:draw-image* stream gray-image 250 (- h (clim-image:image-height image) 10)))))
      ;;  (condition (condition)
      ;;    (clim:with-drawing-options (stream :ink +red+)
       ;;     (clim:draw-text* stream "Error" 20 h)
          ;;     (setf h (+ h 30))))))))
          )))

(define-raster-image-test "09) stencil - 2 dim array - coercion " (stream)
    "Simple loading and coercion."
  (let ((h 10)
        (w 10))
    (dolist (file *opticl-testing-image-files*)
      ;;(handler-case
          (let ((path
                 (uiop/pathname:merge-pathnames* file *opticl-testing-image-directory*)))
            (let ((image (clim-image:read-image path)))
              (clim-image:draw-image* stream image 10 h)
              (setf h (+ h (clim-image:image-height image) 10))
              (let* ((stencil-image (clim-image:coerce-image image 'clim-image:stencil-image))
                     (gray-image (clim-image:coerce-image stencil-image 'clim-image:gray-image)))
                (clim-image:draw-image* stream gray-image 250 (- h (clim-image:image-height image) 10)))))
      ;;  (condition (condition)
      ;;    (clim:with-drawing-options (stream :ink +red+)
       ;;     (clim:draw-text* stream "Error" 20 h)
          ;;     (setf h (+ h 30))))))))
          )))
