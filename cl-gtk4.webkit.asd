(defsystem cl-gtk4.webkit
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "LGPLv3"
  :description "WebKitGTK bindings for Common Lisp."
  :homepage "https://github.com/bohonghuang/cl-gtk4"
  :bug-tracker "https://github.com/bohonghuang/cl-gtk4/issues"
  :source-control (:git "https://github.com/bohonghuang/cl-gtk4.git")
  :serial t
  :components ((:file "webkit"))
  :depends-on (#:cl-gobject-introspection-wrapper #:cl-gtk4))

(uiop:register-image-restore-hook
 (lambda ()
   (let ((package (find-package :webkit)))
     (when package
       (setf (symbol-value (find-symbol "*NS*" package))
             (uiop:symbol-call :gir :require-namespace "WebKit" "6.0"))))))

(defsystem cl-gtk4.webkit/example
  :depends-on (#:asdf
               #:cl-gtk4
               #:cl-gtk4.webkit)
  :build-operation program-op
  :build-pathname "cl-gtk4-webkit-example"
  :entry-point "webkit.example:main"
  :pathname "examples/"
  :components ((:file "webkit")))
