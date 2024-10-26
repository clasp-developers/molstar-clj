(in-package #:molstar)

(jupyter/widgets:defwidget viewer (jupyter/widgets:dom-widget)
  ((layout/show-controls :accessor layout/show-controls
                         :initarg :layout/show-controls
                         :initform t
                         :trait :bool
                         :documentation "")
   (callbacks-lock :accessor callbacks-lock)
   (callbacks :reader callbacks
              :initform (make-hash-table :test #'equal)))
  (:documentation "")
  (:default-initargs :%model-name "ViewerModel"
                     :%model-module +module-name+
                     :%model-module-version +module-version+
                     :%view-name "ViewerView"
                     :%view-module +module-name+
                     :%view-module-version +module-version+))

(defmethod initialize-instance :after ((instance viewer) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (callbacks-lock instance) (bordeaux-threads:make-lock (jupyter:comm-id instance))))

(defun log (instance level text)
  (jupyter/widgets:send-custom instance
                               `(:object-plist "do" "log"
                                               "level" ,(ecase level
                                                          (:message "message")
                                                          (:info "info")
                                                          (:warn "warn")
                                                          (:error "error"))
                                               "text" ,text)))

(defun load-pdb (instance pdb &optional options)
  (jupyter/widgets:send-custom instance
                               `(:object-plist "do" "load_pdb"
                                               "pdb" ,pdb)))

(defun load-structure-from-data (instance data format &key label preset)
  (jupyter/widgets:send-custom instance
                               `(:object-plist "do" "load_structure_from_data"
                                               "data" ,data
                                               "format" ,format
                                               "options" (:object-plist ,@(when label
                                                                            `("label" ,label))
                                                                        ,@(when preset
                                                                            `("preset" ,preset))))))

(defun load-trajectory (instance
                        &key model-data model-url
                          model-format model-binary
                          coordinates-data coordinates-url
                          coordinates-format coordinates-binary)
  (let ((buffers nil))
    (when coordinates-data
      (push coordinates-data buffers)
      (setf coordinates-data (1- (length buffers))))
    (when model-data
      (push model-data buffers)
      (setf model-data (1- (length buffers))))
    (jupyter/widgets:send-custom instance
                                 `(:object-plist
                                   "do" "load_trajectory"
                                   "params" (:object-plist
                                             "model" (:object-plist
                                                      ,@(when model-data
                                                          `("kind" "model-data"
                                                            "data" ,model-data
                                                            "format" ,model-format
                                                            "isBinary" ,model-binary))
                                                      ,@(when model-url
                                                          `("kind" "model-url"
                                                            "url" ,model-url
                                                            "format" ,model-format
                                                            "isBinary" ,model-binary)))
                                             "coordinates" (:object-plist
                                                            ,@(when coordinates-data
                                                                `("kind" "coordinates-data"
                                                                  "data" ,coordinates-data
                                                                  "format" ,coordinates-format
                                                                  "isBinary" ,coordinates-binary))
                                                            ,@(when coordinates-url
                                                                `("kind" "coordinates-url"
                                                                  "url" ,coordinates-url
                                                                  "format" ,coordinates-format
                                                                  "isBinary" ,coordinates-binary)))))
                                 (nreverse buffers))))
