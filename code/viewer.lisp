(in-package #:molstar)

(defvar +simple-panel-values+
  `(("hidden" . :hidden)
    ("full" . :full)))

(defmethod jw::serialize-trait (object (type (eql :simple-panel)) name (value (eql :hidden)))
  (declare (ignore object type name))
  "hidden")

(defmethod jw::serialize-trait (object (type (eql :simple-panel)) name (value (eql :full)))
  (declare (ignore object type name))
  "full")

(defmethod jw::deserialize-trait (object (type (eql :simple-panel)) name value)
  (declare (ignore object type name))
  (cdr (assoc value +simple-panel-values+ :test #'string-equal)))

(defvar +collapsible-panel-values+
  `(("hidden" . :hidden)
    ("collapsed" . :collapsed)
    ("full" . :full)))

(defmethod jw::serialize-trait (object (type (eql :collapsible-panel)) name (value (eql :hidden)))
  (declare (ignore object type name))
  "hidden")

(defmethod jw::serialize-trait (object (type (eql :collapsible-panel)) name (value (eql :collapsed)))
  (declare (ignore object type name))
  "collapsed")

(defmethod jw::serialize-trait (object (type (eql :collapsible-panel)) name (value (eql :full)))
  (declare (ignore object type name))
  "full")

(defmethod jw::deserialize-trait (object (type (eql :collapsible-panel)) name value)
  (declare (ignore object type name))
  (cdr (assoc value +collapsible-panel-values+ :test #'string-equal)))


(jupyter/widgets:defwidget viewer (jupyter/widgets:dom-widget)
  ((layout/is-expanded :accessor layout/expandedp
                       :initarg :layout/expanded
                       :initform nil
                       :trait :bool
                       :documentation "")
   (layout/show-controls :accessor layout/show-controls
                         :initarg :layout/show-controls
                         :initform t
                         :trait :bool
                         :documentation "")
   (layout/left-panel :accessor layout/left-panel
                      :initarg :layout/left-panel
                      :initform :full
                      :trait :collapsible-panel
                      :documentation "")
   (layout/right-panel :accessor layout/right-panel
                       :initarg :layout/right-panel
                       :initform :full
                       :trait :simple-panel
                       :documentation "")
   (layout/top-panel :accessor layout/top-panel
                     :initarg :layout/top-panel
                     :initform :full
                     :trait :simple-panel
                     :documentation "")
   (layout/bottom-panel :accessor layout/bottom-panel
                        :initarg :layout/bottom-panel
                        :initform :full
                        :trait :simple-panel
                        :documentation "")
   (viewport/show-expand :accessor viewport/show-expand
                         :initarg :viewport/show-expand
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
                               `(:object-plist
                                 "name" "log"
                                 "level" ,(ecase level
                                            (:message "message")
                                            (:info "info")
                                            (:warn "warn")
                                            (:error "error"))
                                 "text" ,text)))

(defun load-pdb (instance pdb &optional options)
  (jupyter/widgets:send-custom instance
                               `(:object-plist "name" "load_pdb"
                                               "pdb" ,pdb)))

(defun load-structure-from-data (instance data format &key label preset binary)
  (jupyter/widgets:send-custom instance
                               `(:object-plist
                                 "name" "load_structure_from_data"
                                 "data" 0
                                 "format" ,format
                                 "isBinary" ,binary
                                 "options" (:object-plist
                                            ,@(when label
                                                `("label" ,label))
                                            ,@(when preset
                                                `("preset" ,preset))))
                               (list data)))

#|(defun load-trajectory (instance
                        &key structure-data structure-url model-label
                          struct-format model-binary
                          coordinates-data coordinates-url coordinates-label
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
                                   "name" "load_trajectory"
                                   ,@(when model-label
                                       `("model_label" ,model-label))
                                   ,@(when coordinates-label
                                       `("coordinates_label" ,coordinates-label))
                                   ,@(when model-data
                                       "structure_data" ,model-data
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
                                                               "isBinary" ,coordinates-binary))))
                                 (nreverse buffers))))|#
