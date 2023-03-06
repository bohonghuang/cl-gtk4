;;;; examples/gtk4.lisp

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

(defpackage gtk4.example
  (:use #:cl #:gtk4)
  (:export #:simple #:fibonacci #:menu-test))

(in-package #:gtk4.example)

(define-application (:name simple
                     :id "org.bohonghuang.gtk4-example.counter")
  (define-main-window (window (make-application-window :application *application*))
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
    (unless (widget-visible-p window)
      (window-present window))))

(define-application (:name fibonacci
                     :id "org.bohonghuang.gtk4-example.fibonacci")
  (defun fib (n)
    (if (<= n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))
  (define-main-window (window (make-application-window :application *application*))
    (let ((n 40))
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
                                             (run-in-main-event-loop ()
                                               (setf (button-label button) "Calculating..."
                                                     (widget-sensitive-p button) nil))
                                             (let ((result (fib n)))
                                               (run-in-main-event-loop ()
                                                 (setf (label-text label) (format nil "~A" result)
                                                       (button-label button) "Calculate"
                                                       (widget-sensitive-p button) t))))))))
            (box-append box button)))
        (setf (window-child window) box)))
    (unless (widget-visible-p window)
      (window-present window))))

(define-application (:name menu-test
                     :id "org.bohonghuang.gtk4-example.menu-test")
  (defun menu-test-menu ()
    (let ((menu (gio:make-menu)))
      (let ((submenu (gio:make-menu)))
        (gio:menu-append-item submenu (gio:make-menu-item :model menu :label "Open" :detailed-action "app.open"))
        (gio:menu-append-item submenu (gio:make-menu-item :model menu :label "Exit" :detailed-action "app.exit"))
        (gio:menu-append-submenu menu "File" submenu))
      (let ((submenu (gio:make-menu)))
        (gio:menu-append-item submenu (gio:make-menu-item :model menu :label "About" :detailed-action "app.about"))
        (gio:menu-append-submenu menu "Help" submenu))
      (values menu)))
  (defun menu-test-about-dialog ()
    (let ((dialog (make-about-dialog))
          (system (asdf:find-system :cl-gtk4)))
      (setf (about-dialog-authors dialog) (list (asdf:system-author system))
            (about-dialog-website dialog) (asdf:system-homepage system)
            (about-dialog-version dialog) (asdf:component-version system)
            (about-dialog-program-name dialog) (asdf:component-name system)
            (about-dialog-comments dialog) (asdf:system-description system)
            (about-dialog-logo-icon-name dialog) "application-x-addon")
      (values dialog)))
  (define-main-window (window (make-application-window :application *application*))
    (setf (window-title window) "Menu Test")
    (let ((header-bar (make-header-bar)))
      (let ((menu-button (make-menu-button)))
        (setf (menu-button-menu-model menu-button) (menu-test-menu)
              (button-icon-name menu-button) "open-menu-symbolic")
        (header-bar-pack-end header-bar menu-button))
      (setf (window-titlebar window) header-bar))
    (let ((action (gio:make-simple-action :name "exit"
                                          :parameter-type nil)))
      (gio:action-map-add-action *application* action)
      (connect action "activate"
               (lambda (action param)
                 (declare (ignore action param))
                 (gtk::destroy-all-windows-and-quit))))
    (let ((action (gio:make-simple-action :name "about"
                                          :parameter-type nil)))
      (gio:action-map-add-action *application* action)
      (connect action "activate"
               (lambda (action param)
                 (declare (ignore action param))
                 (let ((dialog (menu-test-about-dialog)))
                   (setf (window-modal-p dialog) t
                         (window-transient-for dialog) window)
                   (window-present dialog)))))
    (let ((window-box (make-box :orientation +orientation-vertical+
                                :spacing 0)))
      (let ((menu-bar (make-popover-menu-bar :model (menu-test-menu))))
        (box-append window-box menu-bar))
      (let ((empty-box (make-box :orientation +orientation-vertical+
                                 :spacing 0)))
        (setf (widget-size-request empty-box) '(400 200))
        (box-append window-box empty-box))
      (setf (window-child window) window-box))
    (unless (widget-visible-p window)
      (window-present window))))
