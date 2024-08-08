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

(defun log/message (instance text)
  (jupyter/widgets:send-custom instance
                               `(:object-plist "do" "log"
                                               "level" "message"
                                               "text" ,text)))

(defun log/info (instance text)
  (jupyter/widgets:send-custom instance
                               `(:object-plist "do" "log"
                                               "level" "info"
                                               "text" ,text)))
(defun log/warn (instance text)
  (jupyter/widgets:send-custom instance
                               `(:object-plist "do" "log"
                                               "level" "warn"
                                               "text" ,text)))

(defun log/error (instance text)
  (jupyter/widgets:send-custom instance
                               `(:object-plist "do" "log"
                                               "level" "error"
                                               "text" ,text)))

(defun load-pdb (instance pdb &optional options)
  (jupyter/widgets:send-custom instance
                               `(:object-plist "do" "load_pdb"
                                               "pdb" ,pdb)))

(defun load-structure-from-data (instance data format &optional options)
  (jupyter/widgets:send-custom instance
                               `(:object-plist "do" "load_structure_from_data"
                                               "data" ,data
                                               "format" ,format)))
