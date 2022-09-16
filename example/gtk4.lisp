;;;; example/gtk4.lisp

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

(defpackage gtk4.example
  (:use #:cl #:gtk4)
  (:export #:main))

(in-package #:gtk4.example)

(defun main ()
  (let ((app (make-application :application-id "org.bohonghuang.cl-gtk4-example"
                               :flags gio:+application-flags-flags-none+)))
    (connect app "activate"
             (lambda (app)
               (let ((window (make-application-window :application app))
                     (box (make-box :orientation +orientation-vertical+
                                    :spacing 4)))
                 (setf (window-title window) "CL-GTK4 Example")
                 (setf (window-child window) box)
                 (let ((label (make-label :str "0")))
                   (setf (widget-hexpand-p label) t)
                   (setf (widget-vexpand-p label) t)
                   (box-append box label)
                   (let ((button (make-button-with-label "Add"))
                         (count 0))
                     (box-append box button)
                     (connect button "clicked" (lambda (button)
                                                 (declare (ignore button))
                                                 (setf (label-text label) (format nil "~A" (incf count))))))
                   (let ((button (make-button-with-label "Exit")))
                     (box-append box button)
                     (connect button "clicked" (lambda (button)
                                                 (declare (ignore button))
                                                 (window-destroy window)))))
                 (window-present window))))
    (gio:application-run app nil)))
