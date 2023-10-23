(defsystem cl-gtk4.sourceview
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "LGPLv3"
  :description "GtkSourceView bindings for Common Lisp."
  :homepage "https://github.com/bohonghuang/cl-gtk4"
  :bug-tracker "https://github.com/bohonghuang/cl-gtk4/issues"
  :source-control (:git "https://github.com/bohonghuang/cl-gtk4.git")
  :serial t
  :components ((:file "sourceview"))
  :depends-on (#:cl-gobject-introspection-wrapper #:cl-gtk4))

(uiop:register-image-restore-hook
 (lambda ()
   (let ((package (find-package :sourceview)))
     (when package
       (setf (symbol-value (find-symbol "*NS*" package))
             (uiop:symbol-call :gir :require-namespace "GtkSource" "5"))))))

(defsystem cl-gtk4.sourceview/example
  :depends-on (#:asdf
               #:cl-gtk4
               #:cl-gtk4.sourceview)
  :build-operation program-op
  :build-pathname "cl-gtk4-sourceview-example"
  :entry-point "sourceview.example:main"
  :pathname "examples/"
  :components ((:file "sourceview")))
