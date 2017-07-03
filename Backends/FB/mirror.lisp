(in-package :clim-fb)

(defclass fb-mirror (opticl-rgb-image-mirror-mixin)
  ((buffer :initform nil
           :accessor fb-mirror-buffer)
   (real-mirror :initform nil
                :initarg :real-mirror
                :accessor fb-mirror-real-mirror)
   (dirty-region-set :initform +nowhere+
                     :accessor fb-mirror-dirty-region-set)))

(defgeneric fb-mirror-create-buffer (fb-mirror width height))
(defgeneric fb-mirror-destroy-buffer (fb-mirror))
(defgeneric fb-mirror-copy-to-buffer (fb-mirror image region-set))
(defgeneric fb-mirror-flush-buffer (fb-mirror region-set))
(defgeneric fb-mirror-set-wm-size-hints (fb-mirror width height space-requirement))

(defmethod %mirror-force-output ((mirror fb-mirror))
  (with-slots (mcclim-render::image-lock mcclim-render::dirty-region)
      mirror
    (when mcclim-render::dirty-region
      (climi::with-lock-held (mcclim-render::image-lock)
        (when mcclim-render::dirty-region
          (setf (fb-mirror-dirty-region-set mirror)
                (region-union (fb-mirror-dirty-region-set mirror)
                              mcclim-render::dirty-region))
          (fb-mirror-copy-to-buffer mirror (mcclim-render::image-mirror-image mirror)
                                    mcclim-render::dirty-region)
          (setf mcclim-render::dirty-region nil))))))

(defmethod %create-mirror-image :after ((mirror fb-mirror) width height)
  (with-slots (mcclim-render::dirty-region mcclim-render::image-lock)
      mirror
    (setf mcclim-render::dirty-region nil)
    (setf (fb-mirror-dirty-region-set mirror) +nowhere+)
    (climi::with-lock-held (mcclim-render::image-lock)
      (fb-mirror-create-buffer mirror width height))))

(defgeneric fb-mirror-flush (fb-mirror)
  (:method ((mirror fb-mirror))
    (with-slots (mcclim-render::image-lock)
        mirror
      (let ((region-set (fb-mirror-dirty-region-set mirror)))
        (when (not (region-equal region-set +nowhere+))
          (let ((reg))
            (climi::with-lock-held (mcclim-render::image-lock)
              (setf reg region-set)
              (setf (fb-mirror-dirty-region-set mirror) +nowhere+)
              (fb-mirror-flush-buffer mirror reg)))))))
  (:method (mirror)
    #+(or) (warn "CLIM-FB::FB-MIRROR-FLUSH no applicable when called with ~A" (class-of mirror))))
