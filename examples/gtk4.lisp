;;;; examples/gtk4.lisp

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
  (:export #:simple #:fibonacci))

(in-package #:gtk4.example)

(defun simple ()
  (let ((app (make-application :application-id "org.bohonghuang.cl-gtk4-example"
                               :flags gio:+application-flags-flags-none+)))
    (connect app "activate"
             (lambda (app)
               (let ((window (make-application-window :application app)))
                 (setf (window-title window) "Simple Counter")
                 (let ((box (make-box :orientation +orientation-vertical+
                                      :spacing 4)))
                   (let ((label (make-label :str "0")))
                     (setf (widget-hexpand-p label) t
                           (widget-vexpand-p label) t)
                     (box-append box label)
                     (let ((button (make-button :label "Add"))
                           (count 0))
                       (connect button "clicked" (lambda (button)
                                                   (declare (ignore button))
                                                   (setf (label-text label) (format nil "~A" (incf count)))))
                       (box-append box button))
                     (let ((button (make-button :label "Exit")))
                       (connect button "clicked" (lambda (button)
                                                   (declare (ignore button))
                                                   (window-destroy window)))
                       (box-append box button)))
                   (setf (window-child window) box))
                 (window-present window))))
    (gio:application-run app nil)))

(defun fibonacci ()
  (labels ((fib (n)
             (if (<= n 2)
                 1
                 (+ (fib (- n 1)) (fib (- n 2))))))
    (let ((app (make-application :application-id "org.bohonghuang.cl-gtk4-example"
                                 :flags gio:+application-flags-flags-none+))
          (n 40))
      (connect app "activate"
               (lambda (app)
                 (let ((window (make-application-window :application app)))
                   (setf (window-title window) "FIBONACCI CALCULATOR")
                   (let ((box (make-box :orientation +orientation-vertical+
                                        :spacing 4)))
                     (let ((label (make-label :str "0")))
                       (setf (widget-hexpand-p label) t
                             (widget-vexpand-p label) t)
                       (box-append box label)
                       (let ((parent box)
                             (box (make-box :orientation +orientation-horizontal+
                                            :spacing 4)))
                         (setf (widget-hexpand-p box) t
                               (widget-halign box) +align-center+)
                         (let ((label (make-label :str "n: ")))
                           (box-append box label))
                         (let ((entry (make-entry)))
                           (setf (widget-hexpand-p label) t
                                 (widget-halign label) +align-fill+
                                 (entry-buffer-text (entry-buffer entry)) (format nil "~A" n))
                           (connect entry "changed" (lambda (entry)
                                                      (setf n (ignore-errors (parse-integer (entry-buffer-text (entry-buffer entry)))))))
                           (box-append box entry))
                         (box-append parent box))
                       (let ((button (make-button :label "Calculate")))
                         (connect button "clicked" (lambda (button)
                                                     (bt:make-thread
                                                      (lambda ()
                                                        (when n
                                                          (glib:with-main-event-loop
                                                            (setf (button-label button) "Calculating..."
                                                                  (widget-sensitive-p button) nil))
                                                          (glib:funcall (lambda (result)
                                                                          (setf (label-text label) (format nil "~A" result)
                                                                                (button-label button) "Calculate"
                                                                                (widget-sensitive-p button) t))
                                                                        (fib n)))))))
                         (box-append box button)))
                     (setf (window-child window) box))
                   (window-present window))))
      (gio:application-run app nil))))
