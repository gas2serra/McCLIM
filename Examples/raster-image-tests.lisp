(in-package :clim-demo)

(defparameter *raster-image-tests* (make-hash-table :test 'equal))

(defparameter *raster-image-width* 510)
(defparameter *raster-image-height* 700)
(defparameter *raster-image-border-width* 5)

(defparameter *testing-image-directory* (uiop/pathname:merge-pathnames* "Examples/images/" (asdf:system-source-directory (asdf:find-system :mcclim))))
(defparameter *testing-image-files* '("RGBXPLORER8.png"
                                      "White_Balance_RGB.png"
                                      "MicroGrayTest.png"))

(defparameter *testing-image-rgb-file* "RGBXPLORER8.png")
(defparameter *testing-image-bn1-file* "White_Balance_RGB.png")
(defparameter *testing-image-bn2-file* "MicroGrayTest.png")

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
              (clim:draw-rectangle* output 0 0 *raster-image-width* *raster-image-height* :filled t
                                    :ink clim:+grey90+)
              (funcall (raster-image-test-drawer item) output))
            (handler-case
                (clim:with-drawing-options (output :clipping-region
                                                   (clim:make-rectangle* 0 0 *raster-image-width* *raster-image-height*))
                  (clim:draw-rectangle* output 0 0 *raster-image-width* *raster-image-height* :filled t
                                        :ink clim:+grey90+)
                  (funcall (raster-image-test-drawer item) output))
              (condition (condition)
                (clim:with-drawing-options (description :ink +red+)
                  (format description "Error:~a~%" condition)))))))))


(defun run-raster-image-tests ()
  (run-frame-top-level
   (make-application-frame
    'raster-image-tests)))

(defun raster-image-test-make-rgb-image-2d (w h color)
  (let* ((image (clim-image:make-image :rgb w h :two-dim-array))
         (pixels (clim-image:image-pixels image)))
    (dotimes (x w)
      (dotimes (y h)
        (setf (aref pixels y x) color)))
    image))

(defun raster-image-test-make-rgb-image-op (w h color)
  (let* ((image (clim-image:make-image :rgb w h :opticl))
         (pixels (clim-image:image-pixels image)))
    (let ((b (first color))
          (g (second color))
          (r (third color)))
      (dotimes (x w)
        (dotimes (y h)
          (setf (opticl:pixel pixels y x) (values r g b)))))
    image))

(defun raster-image-test-make-rgb-image (image-class w h color)
  (let* ((image (make-instance image-class :width w :height h))
         (fn (clim-image:image-rgb-set-fn image)))
    (multiple-value-bind (r g b)
        (clim-image:color->octets color)
      (dotimes (x w)
        (dotimes (y h)
          (funcall fn x y r g b))))
    image))

(defun raster-image-test-make-rgba-image (image-class w h color)
  (let* ((image (make-instance image-class :width w :height h))
         (fn (clim-image:image-rgba-set-fn image)))
    (multiple-value-bind (r g b)
        (clim-image:color->octets color)
      (dotimes (x w)
        (dotimes (y h)
          (funcall fn x y r g b 255))))
    image))

(defun raster-image-test-01-2d (stream)
  (flet ((draw-rect (color w h)
           (let ((image (raster-image-test-make-rgb-image-2d 90 70 color)))
             (clim-image:draw-image* stream image w h))))
    (draw-rect #xFFFFFF 10 10)
    (draw-rect #x000000 110 10)
    (draw-rect #x0000F0 10 100)
    (draw-rect #x00F000 110 100)
    (draw-rect #xF00000 210 100)
    (draw-rect #x800080 10 200)
    (draw-rect #x808000 110 200)))

(defun raster-image-test-01-op (stream)
  (flet ((draw-rect (color w h)
           (let ((image (raster-image-test-make-rgb-image-op 90 70 color)))
             (clim-image:draw-image* stream image w h))))
    (draw-rect (list #xFF #xFF #xFF) 10 10)
    (draw-rect (list #x00 #x00 #x00) 110 10)
    (draw-rect (list #x00 #x00 #xF0) 10 100)
    (draw-rect (list #x00 #xF0 #x00) 110 100)
    (draw-rect (list #xF0 #x00 #x00) 210 100)
    (draw-rect (list #x80 #x00 #x80) 10 200)
    (draw-rect (list #x80 #x80 #x00) 110 200)))

(defun raster-image-test-02 (stream image-class)
  (flet ((draw-rect (color w h)
           (let ((image (raster-image-test-make-rgb-image image-class 90 70 color)))
             (clim-image:draw-image* stream image w h))))
    (draw-rect +white+ 10 10)
    (draw-rect +black+ 110 10)
    (draw-rect +red+ 10 100)
    (draw-rect +green+ 110 100)
    (draw-rect +blue+ 210 100)
    (draw-rect +purple+ 10 200)
    (draw-rect +olive-drab+ 110 200)))

(defun raster-image-test-03 (stream image-class h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (clim-image:coerce-image
                  (clim-image:read-image path :image-class :rgb)
                  image-class)))
      (clim-image:draw-image* stream image 10 h))))

(defun raster-image-test-04 (stream image-class h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-bn2-file* *testing-image-directory*)))
    (let ((image (clim-image:coerce-image
                  (clim-image:read-image path :image-class :gray)
                  :default image-class)))
      (clim-image:draw-image* stream
                              (clim-image:coerce-image image :rgb)
                              10 10)
      (clim-image:draw-image* stream
                              (clim-image:coerce-image image :gray)
                              10 360))))

(defun raster-image-test-05 (stream image-class transformation)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (clim-image:coerce-image
                  (clim-image:read-image path :image-class :rgb)
                  image-class)))
      (clim-image:draw-image* stream image 0 0
                              :transformation transformation))))

(defun raster-image-test-06 (stream image-class transformation clipping-region)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (clim-image:coerce-image
                  (clim-image:read-image path :image-class :rgb)
                  image-class)))
      (with-bounding-rectangle* (x1 y1 x2 y2)
          clipping-region
        (draw-rectangle* stream x1 y1 x2 y2 :ink +green+ :filled nil))
      (with-bounding-rectangle* (x1 y1 x2 y2)
          (transform-region transformation clipping-region)
        (draw-rectangle* stream x1 y1 x2 y2 :ink +blue+ :filled nil))
      (clim-image:draw-image* stream image 0 0
                              :transformation transformation
                              :clipping-region clipping-region))))

(defun raster-image-test-08 (stream image-class h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (clim-image:coerce-image
                  (clim-image:read-image path :image-class :rgb)
                  image-class)))
      (draw-design stream (clim-image:make-image-design image) :x 10 :y h))))

(defun raster-image-test-09 (stream image-class transformation)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (clim-image:coerce-image
                  (clim-image:read-image path :image-class :rgb)
                  image-class)))
      (draw-design stream (clim-image:make-image-design image) :x 0 :y 0
                   :transformation transformation))))

(defun raster-image-test-10 (stream image-class transformation clipping-region)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (clim-image:coerce-image
                  (clim-image:read-image path :image-class :rgb)
                  image-class)))
      (with-bounding-rectangle* (x1 y1 x2 y2)
          clipping-region
        (draw-rectangle* stream x1 y1 x2 y2 :ink +green+ :filled nil))
      (with-bounding-rectangle* (x1 y1 x2 y2)
          (transform-region transformation clipping-region)
        (draw-rectangle* stream x1 y1 x2 y2 :ink +blue+ :filled nil))
      (draw-design stream (clim-image:make-image-design image) :x 0 :y 0
                   :transformation transformation
                   :clipping-region clipping-region))))

(defun raster-image-test-12 (stream image-class w h color)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-bn2-file* *testing-image-directory*)))
    (let* ((alpha-image
            (clim-image:coerce-alpha-channel 
              (clim-image:read-image path)))
           (image (raster-image-test-make-rgba-image image-class
                                                     (clim-image:image-width alpha-image)
                                                     (clim-image:image-height alpha-image)
                                                     color)))
      (clim-image:copy-alpha-channel alpha-image
                             0 0
                             (clim-image:image-width alpha-image)
                             (clim-image:image-height alpha-image)
                             image
                             0 0)
      (clim-image:draw-image* stream image w h))))

(defun raster-image-test-13 (stream fg-image-class bg-image-class w h alpha)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-bn2-file* *testing-image-directory*)))
    (let* ((alpha-image
            (clim-image:coerce-alpha-channel 
              (clim-image:read-image path)))
           (image
            (clim-image:coerce-image (raster-image-test-make-rgba-image 'clim-image:rgba-image
                                                     (clim-image:image-width alpha-image)
                                                     (clim-image:image-height alpha-image)
                                                     +red+)
                          fg-image-class))
           (bg-image
            (clim-image:coerce-image (raster-image-test-make-rgba-image 'clim-image:rgba-image
                                                             (clim-image:image-width alpha-image)
                                                             (clim-image:image-height alpha-image)
                                                             +yellow+)
                          bg-image-class)))

      (clim-image:copy-alpha-channel alpha-image
                             0 0
                             (clim-image:image-width alpha-image)
                             (clim-image:image-height alpha-image)
                             image
                             0 0)
      (clim-image:blend-image image
                              0 0
                              (clim-image:image-width alpha-image)
                              (clim-image:image-height alpha-image)
                              bg-image
                              0 0 :alpha alpha)
      (clim-image:draw-image* stream bg-image w h))))

(defun raster-image-test-15 (stream image-class cx cy cw ch w h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (clim-image:coerce-image
                  (clim-image:read-image path :image-class :rgb)
                  :default image-class)))
      (clim-image:draw-image* stream
                              (clim-image:crop-image image cx cy cw ch)
                              w h))))

(defun raster-image-test-16 (stream image-class design cx cy cw ch w h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*)))
    (let ((image (clim-image:coerce-image
                  (clim-image:read-image path :image-class :rgb)
                  :default image-class))
          (pixeled-design (clim-image:make-pixeled-design design)))
      (clim-image:fill-image image pixeled-design nil :x cx :y cy :width cw :height ch)
      (clim-image:draw-image* stream
                              image
                              w h))))

(defun raster-image-test-17 (stream image-class design cx cy cw ch w h)
  (let ((path (uiop/pathname:merge-pathnames* *testing-image-rgb-file* *testing-image-directory*))
        (path2 (uiop/pathname:merge-pathnames* *testing-image-bn2-file* *testing-image-directory*)))
    (let ((image (clim-image:coerce-image
                  (clim-image:read-image path :image-class :rgb)
                  :default image-class))
          (pixeled-design (clim-image:make-pixeled-design design))
          (stencil (clim-image:coerce-alpha-channel
                    (clim-image:read-image path2))))
      (clim-image:fill-image image pixeled-design stencil :x cx :y cy :width cw :height ch
                             :stencil-dx (- cx) :stencil-dy (- cy))
      (clim-image:draw-image* stream
                              image
                              w h))))

(define-raster-image-test "2d - 01) simple rgb" (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
  (raster-image-test-01-2d stream))

(define-raster-image-test "op - 01) simple rgb" (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
  (raster-image-test-01-op stream))

(define-raster-image-test "2d - 02) simple rgb" (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
  (raster-image-test-02 stream 'clim-image:rgb-image))

(define-raster-image-test "op - 02) simple rgb" (stream)
    "Simple drawing of two dimensional array of pixels: white, black;
red, green, blue;
purple, olive."
  (raster-image-test-02 stream 'clim-image:opticl-rgb-image))

(define-raster-image-test "op - 03) read rgb" (stream)
    ""
  (raster-image-test-03 stream 'clim-image:opticl-rgb-image 10)
  (raster-image-test-03 stream 'clim-image:opticl-gray-image 360))


(define-raster-image-test "2d - 03) read rgb" (stream)
    ""
  (raster-image-test-03 stream 'clim-image:rgb-image 10)
  (raster-image-test-03 stream 'clim-image:gray-image 360))

(define-raster-image-test "op - 04) read gray" (stream)
    ""
  (raster-image-test-04 stream :opticl 10))

  
(define-raster-image-test "2d - 04) read gray" (stream)
    ""
  (raster-image-test-04 stream :two-dim-array 10))

(define-raster-image-test "op - 05) translate " (stream)
    ""
  (raster-image-test-05 stream 'clim-image:opticl-rgb-image
                        (clim:make-translation-transformation 10 10))
  (raster-image-test-05 stream 'clim-image:opticl-gray-image 
                        (clim:make-translation-transformation 10 360)))
  
(define-raster-image-test "2d - 05) translate" (stream)
    ""
  (raster-image-test-05 stream 'clim-image:rgb-image 
                        (clim:make-translation-transformation 10 10))
  (raster-image-test-05 stream 'clim-image:gray-image
                        (clim:make-translation-transformation 10 360)))

(define-raster-image-test "op - 06) clipping " (stream)
    ""
  (raster-image-test-06 stream 'clim-image:opticl-rgb-image
                        (clim:make-translation-transformation 10 10)
                        (make-rectangle* 50 50 250 250))
  (raster-image-test-06 stream 'clim-image:opticl-gray-image 
                        (clim:make-translation-transformation 10 360)
                        (make-rectangle* 50 50 250 250)))
  
(define-raster-image-test "2d - 06) clipping" (stream)
    ""
  (raster-image-test-06 stream 'clim-image:rgb-image 
                        (clim:make-translation-transformation 10 10)
                        (make-rectangle* 50 50 250 250))
  (raster-image-test-06 stream 'clim-image:gray-image
                        (clim:make-translation-transformation 10 360)
                        (make-rectangle* 50 50 250 250)))

(define-raster-image-test "op - 07) with translation" (stream)
    ""
  (with-translation (stream 10 10)
    (raster-image-test-03 stream 'clim-image:opticl-rgb-image 0))
  (with-translation (stream (- 10) 360)
    (raster-image-test-03 stream 'clim-image:opticl-gray-image 0)))

(define-raster-image-test "2d - 07) with-translation" (stream)
    ""
  (with-translation (stream 10 10)
    (raster-image-test-03 stream 'clim-image:rgb-image 0))
  (with-translation (stream (- 10) 360)
    (raster-image-test-03 stream 'clim-image:gray-image 0)))

(define-raster-image-test "op - 08) design draw" (stream)
    ""
  (raster-image-test-08 stream 'clim-image:opticl-rgb-image 10)
  (raster-image-test-08 stream 'clim-image:opticl-gray-image 360))

(define-raster-image-test "2d - 08) design draw" (stream)
    ""
  (raster-image-test-08 stream 'clim-image:rgb-image 10)
  (raster-image-test-08 stream 'clim-image:gray-image 360))

(define-raster-image-test "op - 09) design translate " (stream)
    ""
  (raster-image-test-09 stream 'clim-image:opticl-rgb-image
                        (clim:make-translation-transformation 10 10))
  (raster-image-test-09 stream 'clim-image:opticl-gray-image 
                        (clim:make-translation-transformation 10 360)))
  
(define-raster-image-test "2d - 09) design translate" (stream)
    ""
  (raster-image-test-09 stream 'clim-image:rgb-image 
                        (clim:make-translation-transformation 10 10))
  (raster-image-test-09 stream 'clim-image:gray-image
                        (clim:make-translation-transformation 10 360)))

(define-raster-image-test "op - 10) design clipping " (stream)
    ""
  (raster-image-test-10 stream 'clim-image:opticl-rgb-image
                        (clim:make-translation-transformation 10 10)
                        (make-rectangle* 50 50 250 250))
  (raster-image-test-10 stream 'clim-image:opticl-gray-image 
                        (clim:make-translation-transformation 10 360)
                        (make-rectangle* 50 50 250 250)))
  
(define-raster-image-test "2d - 10) design clipping" (stream)
    ""
  (raster-image-test-10 stream 'clim-image:rgb-image 
                        (clim:make-translation-transformation 10 10)
                        (make-rectangle* 50 50 250 250))
  (raster-image-test-10 stream 'clim-image:gray-image
                        (clim:make-translation-transformation 10 360)
                        (make-rectangle* 50 50 250 250)))

(define-raster-image-test "op - 11) design with-translation" (stream)
    ""
  (with-translation (stream 10 10)
    (raster-image-test-08 stream 'clim-image:opticl-rgb-image 0))
  (with-translation (stream (- 10) 360)
    (raster-image-test-08 stream 'clim-image:opticl-gray-image 0)))

(define-raster-image-test "2d - 11) design with-translation" (stream)
    ""
  (with-translation (stream 10 10)
    (raster-image-test-08 stream 'clim-image:rgb-image 0))
  (with-translation (stream (- 10) 360)
    (raster-image-test-08 stream 'clim-image:gray-image 0)))

(define-raster-image-test "op - 12) alpha" (stream)
    ""
  (raster-image-test-12 stream 'clim-image:opticl-rgba-image 10 10 +red+)
  (raster-image-test-12 stream 'clim-image:opticl-rgba-image 10 360 +green+))

(define-raster-image-test "2d - 12) alpha" (stream)
    ""
  (raster-image-test-12 stream 'clim-image:rgba-image 10 10 +red+)
  (raster-image-test-12 stream 'clim-image:rgba-image 10 360 +green+))

(define-raster-image-test "op - 13) blend" (stream)
    ""
  (raster-image-test-13 stream
                        'clim-image:opticl-rgba-image 
                        'clim-image:opticl-rgba-image
                        10 10 255)
  (raster-image-test-13 stream
                        'clim-image:opticl-rgba-image 
                        'clim-image:opticl-rgba-image
                        10 360 128))

(define-raster-image-test "2d - 13) blend" (stream)
    ""
  (raster-image-test-13 stream
                        'clim-image:rgba-image
                        'clim-image:rgba-image
                        10 10 255)
  (raster-image-test-13 stream
                        'clim-image:rgba-image
                        'clim-image:rgba-image
                        10 360 128))

(define-raster-image-test "op - 14) blend gray" (stream)
    ""
  (raster-image-test-13 stream
                        'clim-image:opticl-rgba-image
                        'clim-image:opticl-gray-image
                        10 10 255)
  (raster-image-test-13 stream
                        'clim-image:opticl-rgba-image
                        'clim-image:opticl-gray-image
                        10 360 128))

(define-raster-image-test "2d - 14) blend gray" (stream)
    ""
  (raster-image-test-13 stream
                        'clim-image:rgba-image
                        'clim-image:gray-image
                        10 10 255)
  (raster-image-test-13 stream
                        'clim-image:rgba-image
                        'clim-image:gray-image
                        10 360 128))

(define-raster-image-test "op - 14) blend rgb" (stream)
    ""
  (raster-image-test-13 stream
                        'clim-image:opticl-rgba-image
                        'clim-image:opticl-rgb-image
                        10 10 255)
  (raster-image-test-13 stream
                        'clim-image:opticl-rgba-image
                        'clim-image:opticl-rgb-image
                        10 360 128))

(define-raster-image-test "2d - 14) blend rgb" (stream)
    ""
  (raster-image-test-13 stream
                        'clim-image:rgba-image
                        'clim-image:rgb-image
                        10 10 255)
  (raster-image-test-13 stream
                        'clim-image:rgba-image
                        'clim-image:rgb-image
                        10 360 128))

(define-raster-image-test "op - 15) crop" (stream)
    ""
  (raster-image-test-15 stream
                        :opticl
                        100 100 100 150
                        10 10)
  (raster-image-test-15 stream
                        :opticl
                        150 150 150 100
                        200 10))

(define-raster-image-test "2d - 15) crop" (stream)
    ""
  (raster-image-test-15 stream
                        :two-dim-array
                        100 100 100 150
                        10 10)
  (raster-image-test-15 stream
                        :two-dim-array
                        150 150 150 100
                        200 10))
(define-raster-image-test "op - 16) fill color" (stream)
    ""
  (raster-image-test-16 stream
                        :opticl
                        +red+
                        100 100 100 150
                        10 10)
  (raster-image-test-16 stream
                        :opticl
                        (compose-in +green+ (make-opacity 0.5))
                        150 150 150 100
                        10 300))

(define-raster-image-test "2d - 16) fill color" (stream)
    ""
  (raster-image-test-16 stream
                        :two-dim-array
                        +red+
                        100 100 100 150
                        10 10)
  (raster-image-test-16 stream
                        :two-dim-array
                        (compose-in +green+ (make-opacity 0.5))
                        150 150 150 100
                        10 300))
(define-raster-image-test "op - 17) fill stencil" (stream)
    ""
  (raster-image-test-17 stream
                        :opticl
                        +red+
                        100 100 100 150
                        10 10)
  (raster-image-test-17 stream
                        :opticl
                        (compose-in +green+ (make-opacity 0.5))
                        150 150 150 100
                        10 300))

(define-raster-image-test "2d - 17) fill stencil" (stream)
    ""
  (raster-image-test-17 stream
                        :two-dim-array
                        +red+
                        100 100 100 150
                        10 10)
  (raster-image-test-17 stream
                        :two-dim-array
                        (compose-in +green+ (make-opacity 0.5))
                        150 150 150 100
                        10 300))
;;;
;;; indipendent
;;;

(define-raster-image-test "zz - 01) output record :draw nil" (stream)
    ""
  (clim:with-output-to-output-record (stream)
    (with-output-recording-options (stream :record t :draw nil)
      (raster-image-test-08 stream 'clim-image:rgb-image 10)
      (draw-rectangle* stream 10 10 50 50 :ink +green+ :filled t))))

(define-raster-image-test "zz - 01) output record moving" (stream)
    ""
  (let ((record
         (clim:with-output-to-output-record (stream)
           (with-output-recording-options (stream :record t :draw t)
             (raster-image-test-08 stream 'clim-image:rgb-image 10)
             (draw-rectangle* stream 10 10 50 50 :ink +green+ :filled t)))))
    (setf (clim:output-record-position record) (values 10 310))
    (replay record stream)))
