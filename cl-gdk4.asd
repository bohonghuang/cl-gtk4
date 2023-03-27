#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))

(defsystem cl-gdk4
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "LGPLv3"
  :description "GDK4 bindings for Common Lisp."
  :homepage "https://github.com/bohonghuang/cl-gtk4"
  :bug-tracker "https://github.com/bohonghuang/cl-gtk4/issues"
  :source-control (:git "https://github.com/bohonghuang/cl-gtk4.git")
  :serial t
  :components ((:file "gdk4"))
  :depends-on (#:cl-gobject-introspection-wrapper))

(uiop:register-image-restore-hook
 (lambda ()
   (let* ((namespace "Gdk")
          (package (find-package (string-upcase namespace))))
     (when package
       (setf (symbol-value (find-symbol "*NS*" package))
             (uiop:symbol-call :gir :require-namespace namespace "4.0"))))))

(defsystem cl-gdk4/example
  :depends-on (#:asdf
               #:cl-gtk4
               #:cl-gdk4
               #:cl-cairo2)
  :build-operation program-op
  :build-pathname "cl-gdk4-example"
  :entry-point "gdk4.example:cairo-test"
  :pathname "./examples"
  :components ((:file "gdk4-cairo")))
