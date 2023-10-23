;;;; examples/sourceview.lisp

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

(defpackage gtksourceview.example
  (:use #:cl #:gtk4)
  (:nicknames sourceview.example)
  (:local-nicknames (#:sv #:sourceview))
  (:export #:main))

(in-package #:sourceview.example)

(defun system-absolute-pathname (pathname)
  (merge-pathnames pathname (asdf:component-pathname (asdf:find-system '#:cl-gtk4.sourceview/example))))

(define-application (:name main
                     :id "org.bohonghuang.gtksourceview-example")
  (define-main-window (window (make-application-window :application *application*))
    (setf (window-title window) "GtkSourceView Example")
    (let ((scrolled-window (make-scrolled-window)))
      (let ((buffer (sv:make-buffer :language (sv:language-manager-get-language
                                               (sv:make-language-manager) "commonlisp"))))
        (setf (gtk:text-buffer-text buffer) (alexandria:read-file-into-string
                                             (system-absolute-pathname "sourceview.lisp")))
        (block setup-dark-scheme
          (let* ((manager (sv:make-style-scheme-manager))
                 (scheme (sv:style-scheme-manager-get-scheme
                          manager (or (find-if
                                       (alexandria:curry #'search "Adwaita-dark")
                                       (sv:style-scheme-manager-scheme-ids manager))
                                      (find-if
                                       (alexandria:curry #'search "dark")
                                       (sv:style-scheme-manager-scheme-ids manager))
                                      (return-from setup-dark-scheme nil)))))
            (setf (sv:buffer-style-scheme buffer) scheme)))
        (let ((view (sv:make-view :buffer buffer)))
          (setf (sv:view-show-line-numbers-p view) t
                (sv:view-highlight-current-line-p view) t)
          (let ((provider (make-css-provider)))
            (css-provider-load-from-data provider "textview { font-family: Monospace; font-size: 12pt; }")
            (style-context-add-provider (widget-style-context view) provider +style-provider-priority-application+))
          (setf (scrolled-window-child scrolled-window) view)))
      (setf (widget-size-request scrolled-window) '(1000 1000))
      (setf (window-child window) scrolled-window))
    (unless (widget-visible-p window)
      (window-present window))))
