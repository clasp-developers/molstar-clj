(asdf:defsystem "molstar-clj"
  :description "A Mol* widget for Common Lisp Jupyter."
  :version "0.1.0"
  :author "Tarn W. Burton"
  :license "MIT"
  :defsystem-depends-on ("jupyter-lab-extension")
  :depends-on ("common-lisp-jupyter")
  :components ((:jupyter-lab-extension molstar-clj
                :pathname "prebuilt/")
               (:module code
                :serial t
                :components ((:file "packages")
                             (:file "version")
                             (:file "viewer")))))
