;;;; examples/gdk4.lisp

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

(cl:defpackage gdk4.example
  (:use #:cl #:gtk4)
  (:export #:cairo-test #:popover-test))

(cl:in-package #:gdk4.example)

(define-application (:name popover-test
                     :id "org.bohonghuang.gdk4-example.popover-test")
  (define-main-window (window (make-application-window :application *application*))
    (setf (window-title window) "Popover Test")
    (let ((box (make-box :orientation +orientation-vertical+ :spacing 0)))
      (setf (widget-size-request box) '(200 200))
      (let ((controller (make-gesture-click)))
        (connect controller 'pressed (lambda (self n-press x y)
                                       (declare (ignore self n-press))
                                       (let ((popover (make-popover)))
                                         (box-append box popover)
                                         (cffi:with-foreign-object (rect '(:struct gdk4:rectangle))
                                           (cffi:with-foreign-slots ((gdk::x gdk::y gdk::width gdk::height) rect (:struct gdk4:rectangle))
                                             (setf gdk::x (round x)
                                                   gdk::y (round y)
                                                   gdk::width (round 0)
                                                   gdk::height (round 0)))
                                           (setf (popover-child popover) (make-label :str "Popover")
                                                 (popover-pointing-to popover) (gobj:pointer-object rect 'gdk:rectangle))
                                           (popover-popup popover)))))
        ;; The `add_controller' method takes ownership of the `controller',
        ;; but `cl-gobject-introspection' doesn't remove the finalizer for the `controller' automatically,
        ;; so we need to remove it here to avoid memory safety issues at present.
        (assert (tg:cancel-finalization (gobj:object-pointer controller)))
        (widget-add-controller box controller))
      (let ((label (make-label :str "Click to pop up a Popover")))
        (setf (widget-vexpand-p label) t)
        (box-append box label))
      (setf (window-child window) box))
    (unless (widget-visible-p window)
      (window-present window))))
