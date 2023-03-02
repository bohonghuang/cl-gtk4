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

(cl:defpackage gtk4
  (:use)
  (:nicknames #:gtk)
  (:export #:*ns*))

(cl:in-package #:gtk4)

(cl:eval-when (:execute :compile-toplevel :load-toplevel)
  (cl:setf gir-wrapper:*quoted-name-alist* '((("TextBuffer" . "get_insert") . text-buffer-get-insert)
                                             (("Gesture" . "group") . group-gestures)
                                             (("Widget" . "is_sensitive") . widget-is-sensitive-p)
                                             (("Widget" . "is_visible") . widget-is-visible-p)
                                             (("EntryBuffer" . "set_text")))))

(gir-wrapper:define-gir-namespace "Gtk" "4.0")

(cl:defun (cl:setf entry-buffer-text) (value instance)
  (cl:declare (cl:type cl:string value))
  (gir:invoke (instance 'set-text) value (cl:length value)))

(cl:defun (cl:setf widget-margin-all) (value instance)
  (cl:setf (widget-margin-top instance) value
           (widget-margin-bottom instance) value
           (widget-margin-start instance) value
           (widget-margin-end instance) value))

(cl:export 'widget-margin-all)

(cl:defun destroy-all-windows-and-quit ()
  (cl:mapcar (alexandria:compose #'window-close (alexandria:curry #'make-window :pointer))
             (glib:glist-list (application-windows gio:*application*)))
  (idle-add (cl:lambda () (gio:application-quit gio:*application*))))

(cl:defun read-return-value ()
  (cl:format cl:*query-io* "~&Enter the return value: ")
  (cl:finish-output cl:*query-io*)
  (cl:multiple-value-list (cl:eval (cl:read cl:*query-io*))))

(cl:defun attach-restarts (function)
  (cl:lambda (cl:&rest args)
    (cl:restart-case (cl:apply function args)
      (return ()
        :report "Return from current handler."
        (cl:values cl:nil))
      (return-and-abort ()
        :report "Return from current handler and terminate the GTK application."
        (destroy-all-windows-and-quit)
        (cl:values cl:nil))
      (return-value (value)
        :report "Return from current handler with specified value."
        :interactive read-return-value
        (cl:values value))
      (return-value-and-abort (value)
        :report "Return from current handler with specified value and terminate the GTK application."
        :interactive read-return-value
        (destroy-all-windows-and-quit)
        (cl:values value)))))

(cl:defun connect (g-object signal c-handler cl:&key after swapped)
  (gir:connect g-object signal (attach-restarts c-handler) :after after :swapped swapped))

(cl:export 'connect)

(cl:defun idle-add (function cl:&optional (priority glib:+priority-default+))
  (glib:idle-add (attach-restarts function) priority))

(cl:export 'idle-add)

(cl:defun timeout-add (interval function cl:&optional (priority glib:+priority-default+))
  (glib:timeout-add interval (attach-restarts function) priority))

(cl:export 'timeout-add)

(cl:defun timeout-add-seconds (interval function cl:&optional (priority glib:+priority-default+))
  (glib:timeout-add-seconds interval (attach-restarts function) priority))

(cl:export 'timeout-add-seconds)

(cl:defmacro run-in-main-event-loop ((cl:&key (priority 'glib:+priority-default+)) cl:&body body)
  `(idle-add (cl:lambda () ,@body cl:nil) ,priority))

(cl:export 'run-in-main-event-loop)

(cl:setf (cl:fdefinition 'application-run) (cl:fdefinition 'gio:application-run))

(cl:export 'application-run)

(cl:eval-when (:execute :compile-toplevel :load-toplevel)
  (cl:setf gir-wrapper:*quoted-name-alist* cl:nil))
