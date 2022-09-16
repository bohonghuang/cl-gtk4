(defsystem cl-gtk4.adw
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "lgpl3"
  :description "Libadwaita binding for Common Lisp."
  :homepage "https://github.com/BohongHuang/cl-gtk4"
  :bug-tracker "https://github.com/BohongHuang/cl-gtk4/issues"
  :source-control (:git "https://github.com/BohongHuang/cl-gtk4.git")
  :serial t
  :components ((:file "adw"))
  :depends-on (#:cl-gobject-introspection-wrapper #:cl-gtk4))

(uiop:register-image-restore-hook
 (lambda ()
   (let* ((namespace "Adw")
          (package (find-package (string-upcase namespace))))
     (when package
       (setf (symbol-value (find-symbol "*NS*" package))
             (uiop:symbol-call :gir :require-namespace namespace))))))

(defsystem cl-gtk4.adw/example
  :depends-on (#:asdf
               #:cl-gtk4
               #:cl-gtk4.adw
               #:cl-gio)
  :build-operation program-op
  :build-pathname "cl-gtk4-libadwaita-example"
  :entry-point "adw.example:main"
  :pathname "./example"
  :components ((:file "adw")))
