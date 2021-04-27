(in-package :palette)


;;;; RGB

(defun hex->rgb (hexstring)
  (let ((a (subseq hexstring 1 3))
	(b (subseq hexstring 3 5))
	(c (subseq hexstring 5 7)))
    (destructuring-bind (r g b)
	(mapcar (lambda (x) (parse-integer x :radix 16)) (list a b c))
      (make-instance 'rgb-color :red r :green g :blue b))))

(defmethod to-string ((rgb rgb-color))
  (with-slots (red green blue) rgb
    (format nil "\#~2,'0X~2,'0X~2,'0X" red green blue)))

(defmethod print-rgb ((rgb rgb-color))
  (with-slots (red green blue) rgb
    (format t "\#~2,'0X~2,'0X~2,'0X " red green blue)))

(defmethod rgb->hsv ((rgb rgb-color))
  (let* ((r (/ (red rgb) 255.0))
	 (g (/ (green rgb) 255.0))
	 (b (/ (blue rgb) 255.0))
	 (max (max r g b))
	 (min (min r g b))
	 (c (- max min))
	 (v max)
	 (s (if (= 0 v) 0 (/ c v)))
	 (hsv (cond
		((= 0 c) (list 0 s v))
		((= r max) (list (* (mod (/ (- g b) c) 6) 60) s v))
		((= g max) (list (* (+ (/ (- b r) c) 2) 60) s v))
		(T (list (* (+ (/ (- r g) c) 4) 60) s v)))))
    (destructuring-bind (h s v) hsv
      (make-instance 'hsv-color :hue h :sat s :val v))))


;;;; HSV

(defun hex->hsv (hexstring)
  (rgb->hsv (hex->rgb hexstring)))

(defmethod to-string ((hsv hsv-color))
  (to-string (hsv->rgb hsv)))

(defmethod print-hsv ((hsv hsv-color))
  (with-slots (hue sat val) hsv
    (format t "(:hue ~,5f :sat ~,5f :val ~,5f) " hue sat val)))

(defmethod hsv->rgb ((hsv hsv-color))
  (with-slots (hue sat val) hsv
    (let* ((c (* sat val))
	   (m (- val c))
	   (x (* c (- 1 (abs (- (mod (/ hue 60) 2) 1)))))
	   (j (floor (/ hue 60)))
	   (rgb-norm (case j
		       (0 (list (+ c m) (+ x m) m))
		       (1 (list (+ x m) (+ c m) m))
		       (2 (list m (+ c m) (+ x m)))
		       (3 (list m (+ x m) (+ c m)))
		       (4 (list (+ x m) m (+ c m)))
		       (5 (list (+ c m) m (+ x m)))
		       (otherwise (list m m m)))))
      (destructuring-bind (r g b) rgb-norm
	(make-instance 'rgb-color
		       :red (round (* 255 r))
		       :green (round (* 255 g))
		       :blue (round (* 255 b)))))))
