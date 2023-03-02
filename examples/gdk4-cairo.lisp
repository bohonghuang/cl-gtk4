;;;; examples/gdk4-cairo.lisp

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

(cl:defpackage cairo-gobject
  (:use)
  (:export #:*ns*))

(cl:in-package #:cairo-gobject)

(gir-wrapper:define-gir-namespace "cairo")

(cl:defpackage gdk4.example
  (:use #:cl #:gtk4)
  (:export #:cairo-test))

(cl:in-package #:gdk4.example)

(cffi:defcstruct gdk-rgba
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(defmacro with-gdk-rgba ((pointer color) &body body)
  `(locally
       #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
       (cffi:with-foreign-object (,pointer '(:struct gdk-rgba))
         (let ((,pointer (make-instance 'gir::struct-instance
                                        :class (gir:nget gdk::*ns* "RGBA")
                                        :this ,pointer)))
           (gdk:rgba-parse ,pointer ,color)
           (locally
               #+sbcl (declare (sb-ext:unmuffle-conditions sb-ext:compiler-note))
               ,@body)))))

(defun draw-func (area cr width height)
  (declare (ignore area)
           (optimize (speed 3)
                     (debug 0)
                     (safety 0)))
  (let ((width (coerce (the fixnum width) 'single-float))
        (height (coerce (the fixnum height) 'single-float))
        (fpi (coerce pi 'single-float)))
    (let* ((radius (/ (min width height) 2.0))
           (stroke-width (/ radius 8.0))
           (button-radius (* radius 0.4)))
      (declare (type single-float radius stroke-width button-radius))
      (with-gdk-rgba (color "#000000")
        (cairo:arc (/ width 2.0) (/ height 2.0) radius 0.0 (* 2.0 fpi))
        (gdk:cairo-set-source-rgba cr color)
        (cairo:fill-path))
      (with-gdk-rgba (color "#FF0000")
        (cairo:arc (/ width 2.0) (/ height 2.0) (- radius stroke-width) pi (* 2.0 fpi))
        (gdk:cairo-set-source-rgba cr color)
        (cairo:fill-path))
      (with-gdk-rgba (color "#FFFFFF")
        (cairo:arc (/ width 2.0) (/ height 2.0) (- radius stroke-width) 0.0 fpi)
        (gdk:cairo-set-source-rgba cr color)
        (cairo:fill-path))
      (with-gdk-rgba (color "#000000")
        (let ((bar-length (sqrt (- (expt (* radius 2) 2.0) (expt stroke-width 2.0)))))
          (declare (type single-float bar-length))
          (cairo:rectangle (+ (- (/ width 2.0) radius) (- radius (/ bar-length 2.0)))
                           (+ (- (/ height 2.0) radius) (- radius (/ stroke-width 2.0)))
                           bar-length
                           stroke-width))
        (gdk:cairo-set-source-rgba cr color)
        (cairo:fill-path))
      (with-gdk-rgba (color "#000000")
        (cairo:arc (/ width 2.0) (/ height 2.0) button-radius 0.0 (* 2.0 fpi))
        (gdk:cairo-set-source-rgba cr color)
        (cairo:fill-path))
      (with-gdk-rgba (color "#FFFFFF")
        (cairo:arc (/ width 2.0) (/ height 2.0) (- button-radius stroke-width) 0.0 (* 2.0 fpi))
        (gdk:cairo-set-source-rgba cr color)
        (cairo:fill-path)))))

(cffi:defcallback %draw-func :void ((area :pointer)
                                    (cr :pointer)
                                    (width :int)
                                    (height :int)
                                    (data :pointer))
  (declare (ignore data))
  (let ((cairo:*context* (make-instance 'cairo:context
                                        :pointer cr
                                        :width width
                                        :height height
                                        :pixel-based-p nil)))
    (draw-func (make-instance 'gir::object-instance
                              :class (gir:nget gtk:*ns* "DrawingArea")
                              :this area)
               (make-instance 'gir::struct-instance
                              :class (gir:nget cairo-gobject:*ns* "Context")
                              :this cr)
               width height)))

(defun cairo-test ()
  (let ((app (make-application :application-id "org.bohonghuang.cl-gdk4-cairo-example"
                               :flags gio:+application-flags-flags-none+)))
    (connect app "activate"
             (lambda (app)
               (let ((window (make-application-window :application app)))
                 (setf (window-title window) "Drawing Area Test")
                 (let ((area (gtk:make-drawing-area)))
                   (setf (drawing-area-content-width area) 200
                         (drawing-area-content-height area) 200
                         (drawing-area-draw-func area) (list (cffi:callback %draw-func)
                                                             (cffi:null-pointer)
                                                             (cffi:null-pointer)))
                   (setf (window-child window) area))
                 (window-present window))))
    (application-run app nil)))
