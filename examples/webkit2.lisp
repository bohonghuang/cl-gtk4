;;;; examples/webkit2.lisp

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

(defpackage webkit2.example
  (:use #:cl #:gtk4)
  (:export #:main))

(in-package #:webkit2.example)

(defparameter *home-uri* "https://google.com")

(defun main-window (app)
  (let ((window (make-application-window :application app))
        (web-view (webkit:make-web-view :context (webkit:make-web-context))))
    (setf (window-title window) "CL-GTK4-WEBKIT2-EXAMPLE"
          (window-default-size window) '(800 600))
    (connect web-view "load-changed" (lambda (web-view event)
                                       (declare (ignore event))
                                       (setf (window-title window) (if (webkit:web-view-loading-p web-view)
                                                                       (webkit:web-view-uri web-view)
                                                                       (webkit:web-view-title web-view)))))
    (let ((box (make-box :orientation +orientation-vertical+
                         :spacing 0)))
      (let ((toolbar (make-center-box)))
        (widget-add-css-class toolbar "toolbar")

        (let ((box (make-box :orientation +orientation-horizontal+
                             :spacing 4)))
          (let ((button (make-button :icon-name "go-previous-symbolic")))
            (connect button "clicked" (lambda (button)
                                        (declare (ignore button))
                                        (webkit:web-view-go-back web-view)))
            (connect web-view "load-changed" (lambda (web-view event)
                                               (declare (ignore event))
                                               (setf (widget-sensitive-p button) (webkit:web-view-can-go-back-p web-view))))
            (box-append box button))
          (let ((button (make-button :icon-name "go-next-symbolic")))
            (connect button "clicked" (lambda (button)
                                        (declare (ignore button))
                                        (webkit:web-view-go-forward web-view)))
            (connect web-view "load-changed" (lambda (web-view event)
                                               (declare (ignore event))
                                               (setf (widget-sensitive-p button) (webkit:web-view-can-go-forward-p web-view))))
            (box-append box button))
          (let ((button (make-button :icon-name "go-home-symbolic")))
            (connect button "clicked" (lambda (button)
                                        (declare (ignore button))
                                        (webkit:web-view-load-uri web-view *home-uri*)))
            (box-append box button))
          (setf (center-box-start-widget toolbar) box))
        (let ((box (make-box :orientation +orientation-horizontal+
                             :spacing 4)))
          (setf (widget-halign box) +align-fill+
                (widget-hexpand-p box) t
                (widget-margin-start box) 50
                (widget-margin-end box) 50)
          (let ((entry (make-entry)))
            (setf (widget-halign entry) +align-fill+
                  (widget-hexpand-p entry) t)
            (connect entry "activate" (lambda (entry)
                                        (webkit:web-view-load-uri web-view (entry-buffer-text (entry-buffer entry)))))
            (connect web-view "load-changed" (lambda (web-view event)
                                               (declare (ignore event))
                                               (setf (entry-buffer-text (entry-buffer entry)) (webkit:web-view-uri web-view))))
            (box-append box entry))
          (let ((button (make-button :icon-name "view-refresh-symbolic")))
            (connect button "clicked" (lambda (button)
                                        (declare (ignore button))
                                        (if (webkit:web-view-loading-p web-view)
                                            (webkit:web-view-stop-loading web-view)
                                            (webkit:web-view-reload web-view))))
            (connect web-view "load-changed" (lambda (web-view event)
                                               (declare (ignore event))
                                               (setf (button-icon-name button) (if (webkit:web-view-loading-p web-view)
                                                                                   "process-stop-symbolic"
                                                                                   "view-refresh-symbolic"))))
            (box-append box button))
          (setf (center-box-center-widget toolbar) box))
        (box-append box toolbar))
      (let ((progress-bar (make-progress-bar)))
        (widget-add-css-class progress-bar "osd")
        (connect web-view "load-changed" (lambda (web-view event)
                                           (declare (ignore event))
                                           (setf (progress-bar-fraction progress-bar)
                                                 (if (webkit:web-view-loading-p web-view)
                                                     (webkit:web-view-estimated-load-progress web-view)
                                                     0.0d0))))
        (box-append box progress-bar))
      (let ((web-view web-view))
        (setf (widget-vexpand-p web-view) t
              (widget-hexpand-p web-view) t)
        (webkit:web-view-load-uri web-view *home-uri*)
        (box-append box web-view))
      (setf (window-child window) box))
    (window-present window)))

(defun main ()
  (let ((app (make-application :application-id "org.bohonghuang.cl-gtk4-webkit2-example"
                               :flags gio:+application-flags-flags-none+)))
    (connect app "activate" #'main-window)
    (gio:application-run app nil)))
