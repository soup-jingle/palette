(in-package :palette)

(defmethod get-increments ((hsv1 hsv-color) (hsv2 hsv-color) (steps integer))
  (make-instance 'hsv-color
		 :hue (/ (- (hue hsv2) (hue hsv1)) (- steps 1))
		 :sat (/ (- (sat hsv2) (sat hsv1)) (- steps 1))
		 :val (/ (- (val hsv2) (val hsv1)) (- steps 1))))

(defmethod calculate-increment-change ((hsv hsv-color) (inc hsv-color) (step integer))
  (make-instance 'hsv-color
		 :hue (+ (hue hsv) (* step (hue inc)))
 		 :sat (+ (sat hsv) (* step (sat inc)))
		 :val (+ (val hsv) (* step (val inc)))))

(defmethod trace-path ((rgb1 rgb-color) (rgb2 rgb-color) steps)
  (let* ((hsv1 (rgb->hsv rgb1))
	 (hsv2 (rgb->hsv rgb2))
	 (inc (get-increments hsv1 hsv2 steps)))
    (loop for i from 0 below steps
	  collect (hsv->rgb (calculate-increment-change hsv1 inc i)))))

(defmethod rotate ((hsv hsv-color) degrees)
  (with-slots (hue sat val) hsv
    (make-instance 'hsv-color
		   :hue (mod (+ degrees hue) 360)
		   :sat sat
		   :val val)))

(defmethod rotate ((rgb rgb-color) degrees)
  (hsv->rgb (rotate (rgb->hsv rgb) degrees)))

;; (defun rgb-multiply-hsv (rgb &key (h 1) (s 1) (v 1))
;;   (let ((hsv (rgb->hsv rgb)))
;;     (destructuring-bind (old-h old-s old-v) hsv
;; 	(let ((new-h (mod (* h old-h 1.0) 360.0))
;; 	      (new-s (* s old-s 1.0))
;; 	      (new-v (* v old-v 1.0)))
;; 	  (rgb->string*
;; 	   (hsv->rgb (list new-h
;; 			   (if (> new-s 1) 1 new-s)
;; 			   (if (> new-v 1) 1 new-v))))))))

;; (defun rgb-set-hsv (rgb &key h s v)
;;     (destructuring-bind (old-h old-s old-v) (rgb->hsv rgb)
;;       (hsv->rgb (list (or h old-h)
;; 		      (or s old-s)
;; 		      (or v old-v)))))
  
;; (defun color-in-between (hex1 hex2)
;;   (let ((c1 (rgb->hsv (hex->rgb hex1)))
;; 	(c2 (rgb->hsv (hex->rgb hex2))))
;;     (rgb->string* (hsv->rgb
;; 		   (mapcar (lambda (x y) (/ (+ x y) 2))
;; 			   c1 c2)))))

(defun get-more-colors (hexstring &optional (steps 12))
  (loop for i from 0 below steps
	for angle = (* i (/ 360.0 steps))
	collect (to-string (rotate (hex->rgb hexstring) angle))))

(defun get-more-colors-rgb (hexstring &optional (steps 12))
  (loop for i from 0 below steps
	for angle = (* i (/ 360.0 steps))
	collect (rotate (hex->rgb hexstring) angle)))

(defun get-more-colors-hsv (hexstring &optional (steps 12))
  (loop for i from 0 below steps
	for angle = (* i (/ 360.0 steps))
	collect (rotate (hex->hsv hexstring) angle)))

(defun get-more-colors+ (hexstring &optional (steps 12))
  (let ((dark-hsv (rgb->hsv (hex->rgb hexstring))))
    (setf (val dark-hsv) (/ (val dark-hsv) 2))
    (mapcar #'cons
	    (get-more-colors hexstring steps)
	    (get-more-colors (to-string (hsv->rgb dark-hsv)) steps))))


