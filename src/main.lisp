(defpackage palette
  (:use :cl)
  (:export
   :hex->hsv :hex->rgb
   :rgb->hsv :hsv->rgb
   :get-more-colors :which-color))
(in-package :palette)

(defconstant red-midpoint 0)
(defconstant orange-midpoint 30)
(defconstant yellow-midpoint 60)
(defconstant chartreuse-midpoint 90)
(defconstant green-midpoint 120)
(defconstant spring-midpoint 150)
(defconstant cyan-midpoint 180)
(defconstant azure-midpoint 210)
(defconstant blue-midpoint 240)
(defconstant violet-midpoint 270)
(defconstant magenta-midpoint 300)
(defconstant rose-midpoint 330)
;; (defconstant color-midpoints (list red-midpoint orange-midpoint yellow-midpoint chartreuse-midpoint
;; 				   green-midpoint spring-midpoint cyan-midpoint azure-midpoint
;; 				   blue-midpoint violet-midpoint magenta-midpoint rose-midpoint))

;; (defconstant red-p-int 0)
;; (defconstant orange-p-int 1)
;; (defconstant yellow-p-int 2)
;; (defconstant chartreuse-p-int 3)
;; (defconstant green-p-int 4)
;; (defconstant spring-p-int 5)
;; (defconstant cyan-p-int 6)
;; (defconstant azure-p-int 7)
;; (defconstant blue-p-int 8)
;; (defconstant violet-p-int 9)
;; (defconstant magenta-p-int 10)
;; (defconstant rose-p-int 11)

(defclass rgb-color ()
  ((red
    :initarg :red
    :initform 0
    :accessor red)
   (green
    :initarg :green
    :initform 0
    :accessor green)
   (blue
    :initarg :blue
    :initform 0
    :accessor blue)))

(defmethod print-object ((obj rgb-color) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((red red)
		     (green green)
		     (blue blue))
	obj
      (format stream ":red ~d :green ~d :blue ~d" red green blue))))

(defclass hsv-color ()
  ((hue
    :initarg :hue
    :initform 0
    :accessor hue)
   (sat
    :initarg :sat
    :initform 0
    :accessor sat)
   (val
    :initarg :val
    :initform 0
    :accessor val)))

(defmethod print-object ((obj hsv-color) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((hue hue)
		     (sat sat)
		     (val val))
	obj
      (format stream ":hue ~d :sat ~d :val ~d" hue sat val))))

(defgeneric which-color (color))

(defmethod which-color ((color string))
  (let* ((h (hue (hex->hsv color)))
	 (b (mod (floor (/ (round (+ h 15)) 30)) 12)))
    (case b
      (0  :red)
      (1  :orange)
      (2  :yellow)
      (3  :chartreuse)
      (4  :green)
      (5  :spring)
      (6  :cyan)
      (7  :azure)
      (8  :blue)
      (9  :violet)
      (10 :magenta)
      (11 :rose))))
      

(defmethod which-color ((color rgb-color))
  (which-color (to-string color)))

(defmethod which-color ((color hsv-color))
  (which-color (to-string color)))
