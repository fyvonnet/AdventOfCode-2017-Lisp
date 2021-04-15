(defpackage :day20
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :scan-to-strings)
  (:import-from :fset :convert :empty-map :lookup :with)
  (:export main))

(in-package :day20)


(defun parse-line (line)
  (multiple-value-bind (_ matches)
    (scan-to-strings "p=<(.+),(.+),(.+)>, v=<(.+),(.+),(.+)>, a=<(.+),(.+),(.+)>" line)
    (let ((lst (map 'list #'parse-integer matches)))
      (loop for i below 3 collect (subseq lst (* 3 i) (* 3 (1+ i)))))))

(defun numbering (lst &optional (n 0))
  (when lst
    (cons 
      (cons n (car lst))
      (numbering (cdr lst) (1+ n)))))

(defun magnitude (lst)
  (apply #'+ (mapcar #'abs lst)))

(defun compare-magnitude (a b)
  (let
    ((acc-mag-a (magnitude (fourth a)))
     (acc-mag-b (magnitude (fourth b))))
    (if (= acc-mag-a acc-mag-b)
      (let
        ((vel-mag-a (magnitude (third a)))
         (vel-mag-b (magnitude (third b))))
        (if (= vel-mag-a vel-mag-b)
          (< (magnitude (second a)) (magnitude (second b)))
          (< vel-mag-a vel-mag-b)))
      (< acc-mag-a acc-mag-b))))

(defun update-particle (particle)
  (destructuring-bind (pos vel acc) particle
    (let ((new-vel (mapcar #'+ vel acc)))
      (list
        (mapcar #'+ pos new-vel)
        new-vel
        acc))))

(defun collide-particles (particles)
  (remove-if-not
    (lambda (x) (= 3 (length x)))
    (convert
      'list
      (reduce
        (lambda (m p) (with m (car p) (concatenate 'list (cdr p) (lookup m (car p)))))
        (mapcar #'update-particle particles)
        :initial-value (empty-map)))))

(defun move-particles (particles &optional (n 100))
  (if (zerop n)
    (length particles)
    ;particles
    (move-particles (collide-particles particles) (1- n))))

(defun main ()
  (let
    ((input (read-input-as-list 20 #'parse-line)))
    (print (caar (sort (numbering input) #'compare-magnitude)))
    (print (move-particles input))))

