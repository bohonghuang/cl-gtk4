;;;; gtk4.lisp

;;;; Copyright (C) 2022-2023 Bohong Huang
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Lesser General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defpackage gtk4
  (:use #:cl)
  (:nicknames #:gtk)
  (:import-from #:gio #:*application*)
  (:export #:*ns* #:*application*))

(in-package #:gtk4)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (setf gir-wrapper:*quoted-name-alist* '((("TextBuffer" . "get_insert") . text-buffer-get-insert)
                                          (("Gesture" . "group") . group-gestures)
                                          (("Widget" . "is_sensitive") . widget-is-sensitive-p)
                                          (("Widget" . "is_visible") . widget-is-visible-p)
                                          (("EntryBuffer" . "set_text")))))

(gir-wrapper:define-gir-namespace "Gtk" "4.0")

(eval-when (:execute :compile-toplevel :load-toplevel)
  (setf gir-wrapper:*quoted-name-alist* nil))

(defun (setf entry-buffer-text) (value instance)
  (declare (type string value))
  (gir:invoke (instance 'set-text) value (length value)))

(defun (setf widget-margin-all) (value instance)
  (setf (widget-margin-top instance) value
        (widget-margin-bottom instance) value
        (widget-margin-start instance) value
        (widget-margin-end instance) value))

(export 'widget-margin-all)

(defun destroy-all-windows ()
  (mapcar (alexandria:compose #'window-close (alexandria:rcurry #'gobj:pointer-object 'window))
          (glib:glist-list (application-windows gio:*application*))))

(defun destroy-all-windows-and-quit ()
  (destroy-all-windows)
  (idle-add (lambda () (gio:application-quit gio:*application*))))

(defun read-return-value ()
  (format *query-io* "~&Enter the return value: ")
  (finish-output *query-io*)
  (multiple-value-list (eval (read *query-io*))))

(defun attach-restarts (function)
  (lambda (&rest args)
    (restart-case (apply function args)
      (return ()
        :report "Return from current handler."
        (values nil))
      (return-and-abort ()
        :report "Return from current handler and abort the GTK application."
        (destroy-all-windows-and-quit)
        (values nil))
      (return-value (value)
        :report "Return from current handler with specified value."
        :interactive read-return-value
        (values value))
      (return-value-and-abort (value)
        :report "Return from current handler with specified value and abort the GTK application."
        :interactive read-return-value
        (destroy-all-windows-and-quit)
        (values value)))))

(defun connect (g-object signal c-handler &key after swapped)
  (gir:connect g-object signal (attach-restarts c-handler) :after after :swapped swapped))

(export 'connect)

(defun idle-add (function &optional (priority glib:+priority-default+))
  (glib:idle-add (attach-restarts function) priority))

(export 'idle-add)

(defun timeout-add (interval function &optional (priority glib:+priority-default+))
  (glib:timeout-add interval (attach-restarts function) priority))

(export 'timeout-add)

(defun timeout-add-seconds (interval function &optional (priority glib:+priority-default+))
  (glib:timeout-add-seconds interval (attach-restarts function) priority))

(export 'timeout-add-seconds)

(defmacro run-in-main-event-loop ((&key (priority 'glib:+priority-default+)) &body body)
  `(idle-add (lambda () ,@body nil) ,priority))

(export 'run-in-main-event-loop)

(setf (fdefinition 'application-run) (fdefinition 'gio:application-run))

(defun application-run (application argv)
  (handler-bind ((t (lambda (c)
                      (print c))))
    (gio:application-run application argv)))

(export 'application-run)

(defun simple-break-symbol ()
  (find-symbol "SIMPLE-BREAK" (cond
                                ((member :slynk *features*) :slynk)
                                ((member :swank *features*) :swank)
                                (t (return-from simple-break-symbol nil)))))

(defvar *simple-break-function* nil)

(defun break-from-main-event-loop ()
  (if gio:*application*
      (glib:idle-add (lambda ()
                       (restart-case (funcall *simple-break-function*)
                         (abort-application ()
                           :report "Abort the GTK application."
                           (destroy-all-windows-and-quit)))
                       (values nil))
                     glib:+priority-high+)
      (funcall *simple-break-function*)))

(defun install-break-handler ()
  (when *simple-break-function*
    (error "Cannot install the break handler twice."))
  (setf *simple-break-function* (fdefinition (simple-break-symbol))
        (fdefinition (simple-break-symbol)) (fdefinition 'break-from-main-event-loop)))

(export 'install-break-handler)

(defun uninstall-break-handler ()
  (unless *simple-break-function*
    (error "The break handler has not been installed."))
  (setf (fdefinition (simple-break-symbol)) *simple-break-function*
        *simple-break-function* nil))

(export 'uninstall-break-handler)

(when (simple-break-symbol)
  (unless *simple-break-function*
    (install-break-handler)))
