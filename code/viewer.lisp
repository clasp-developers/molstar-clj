(in-package #:molstar)

(jupyter/widgets:defwidget viewer (jupyter/widgets:dom-widget)
  ((callbacks-lock
     :accessor callbacks-lock)
   (callbacks
     :reader callbacks
     :initform (make-hash-table :test #'equal)))
  (:documentation "")
  (:default-initargs
    :%model-name "ViewerModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "ViewerView"
    :%view-module +module-name+
    :%view-module-version +module-version+))

(defmethod initialize-instance :after ((instance viewer) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (callbacks-lock instance) (bordeaux-threads:make-lock (jupyter:comm-id instance))))
