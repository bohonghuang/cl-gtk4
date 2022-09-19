(defsystem cl-gtk4.webkit2
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "lgpl3"
  :description "WebKit2GTK binding for Common Lisp."
  :homepage "https://github.com/BohongHuang/cl-gtk4"
  :bug-tracker "https://github.com/BohongHuang/cl-gtk4/issues"
  :source-control (:git "https://github.com/BohongHuang/cl-gtk4.git")
  :serial t
  :components ((:file "webkit2"))
  :depends-on (#:cl-gobject-introspection-wrapper #:cl-gtk4))

(uiop:register-image-restore-hook
 (lambda ()
   (let ((package (find-package :webkit2)))
     (when package
       (setf (symbol-value (find-symbol "*NS*" package))
             (uiop:symbol-call :gir :require-namespace "WebKit2" "5.0"))))))

(defsystem cl-gtk4.webkit2/example
  :depends-on (#:asdf
               #:cl-gtk4
               #:cl-gtk4.webkit2)
  :build-operation program-op
  :build-pathname "cl-gtk4-webkit2-example"
  :entry-point "webkit2.example:main"
  :pathname "./example"
  :components ((:file "webkit2")))
