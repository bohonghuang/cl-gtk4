;;;; example/adw.lisp

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

(defpackage adw.example
  (:use #:cl #:gtk4)
  (:export #:main))

(in-package #:adw.example)

(defun main-window (app)
  (let ((expression nil))
    (let ((window (adw:make-application-window :app app)))
      (widget-add-css-class window "devel")
      (setf (widget-size-request window) '(400 600))
      (let ((box (make-box :orientation +orientation-vertical+
                           :spacing 0)))
        (setf (adw:window-content window) box)
        (let ((header-bar (adw:make-header-bar)))
          (setf (adw:header-bar-title-widget header-bar)
                (adw:make-window-title :title (lisp-implementation-type)
                                       :subtitle (lisp-implementation-version)))
          (box-append box header-bar))
        (let ((carousel (adw:make-carousel)))
          (setf (widget-hexpand-p carousel) t
                (widget-vexpand-p carousel) t
                (adw:carousel-interactive-p carousel) t)
          (let ((page (adw:make-status-page)))
            (setf (widget-hexpand-p page) t
                  (widget-vexpand-p page) t
                  (adw:status-page-icon-name page) "utilities-terminal-symbolic"
                  (adw:status-page-title page) "Simple Lisp REPL"
                  (adw:status-page-description page) " ")
            (flet ((eval-expression (widget)
                     (declare (ignore widget))
                     (when expression
                       (setf (adw:status-page-description page)
                             (princ-to-string
                              (handler-case (eval expression)
                                (error (err) err)))))))
              (let ((box (make-box :orientation +orientation-vertical+
                                   :spacing 0)))
                (let ((group (adw:make-preferences-group)))
                  (setf (widget-margin-all group) 10)
                  (let ((row (adw:make-action-row)))
                    (setf (adw:preferences-row-title row) (format nil "~A>" (or (car (package-nicknames *package*))
                                                                                (package-name *package*))))
                    (let ((entry (make-entry)))
                      (setf (widget-valign entry) +align-center+
                            (widget-hexpand-p entry) t)
                      (connect entry "changed" (lambda (entry)
                                                 (setf expression (ignore-errors (read-from-string (entry-buffer-text (entry-buffer entry)))))
                                                 (funcall (if expression #'widget-remove-css-class #'widget-add-css-class) entry "error")))
                      (connect entry "activate" #'eval-expression)
                      (adw:action-row-add-suffix row entry))
                    (adw:preferences-group-add group row))
                  (box-append box group))
                (let ((carousel-box box)
                      (box (make-box :orientation +orientation-horizontal+
                                     :spacing 0)))
                  (setf (widget-hexpand-p box) t
                        (widget-halign box) +align-fill+)
                  (let ((button (make-button :label "Exit")))
                    (setf (widget-css-classes button) '("pill")
                          (widget-margin-all button) 10
                          (widget-hexpand-p button) t)
                    (connect button "clicked" (lambda (button)
                                                (declare (ignore button))
                                                (window-destroy window)))
                    (box-append box button))
                  (let ((button (make-button :label "Eval")))
                    (setf (widget-css-classes button) '("suggested-action" "pill")
                          (widget-margin-all button) 10
                          (widget-hexpand-p button) t)
                    (connect button "clicked" #'eval-expression)
                    (box-append box button))
                  (box-append carousel-box box))
                (setf (adw:status-page-child page) box)))
            (adw:carousel-append carousel page))
          (box-append box carousel)))
      (window-present window))))

(defun main ()
  (unless (adw:initialized-p)
    (adw:init))
  (let ((app (make-application :application-id "org.bohonghuang.cl-gtk4-libadwaita-example"
                               :flags gio:+application-flags-flags-none+)))
    (connect app "activate" #'main-window)
    (gio:application-run app nil)))
