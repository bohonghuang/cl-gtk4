;;;; gtk4.lisp

;;;; Copyright (C) 2022 Bohong Huang
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
  (:nicknames #:gtk)
  (:export #:*ns* #:connect))

(cl:in-package #:gtk4)

(cl:eval-when (:execute :compile-toplevel :load-toplevel)
  (cl:setf gir-wrapper:*quoted-name-alist* '(("ATContext" . at-context)
                                             (("TextBuffer" . "get_insert") . text-buffer-get-insert)
                                             (("Gesture" . "group") . group-gestures)
                                             (("Widget" . "is_sensitive") . widget-is-sensitive-p)
                                             (("Widget" . "is_visible") . widget-is-visible-p))))

(gir-wrapper:define-gir-namespace "Gtk" "4.0")

(cl:defun (cl:setf widget-margin-all) (value instance)
  (cl:setf (widget-margin-top instance) value
           (widget-margin-bottom instance) value
           (widget-margin-start instance) value
           (widget-margin-end instance) value))

(cl:export 'widget-margin-all)

(cl:setf (cl:fdefinition 'connect) #'gir:connect)

(cl:eval-when (:execute :compile-toplevel :load-toplevel)
  (cl:setf gir-wrapper:*quoted-name-alist* cl:nil))
