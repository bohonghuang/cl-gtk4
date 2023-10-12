(cl:defpackage gdk-pixbuf2
  (:use)
  (:nicknames #:gdk-pixbuf)
  (:export #:*ns*))

(in-package #:gdk-pixbuf2)

(gir-wrapper:define-gir-namespace "GdkPixbuf" "2.0")
