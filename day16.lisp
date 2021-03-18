(defpackage :day16
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :split :scan-to-strings)
  (:import-from :trivia :match)
  (:import-from :fset :convert :empty-map :lookup :with)
  (:export main))

(in-package :day16)

(defconstant +nprogs+ 16)

(defun parse-move (str)
  (multiple-value-bind (_ matches)
    (scan-to-strings "^(.)(.*?)(\\/(.*))?$" str)
    (case (char (aref matches 0) 0)
      (#\s (list 'spin (parse-integer (aref matches 1))))
      (#\x (cons 'xchange (mapcar (lambda (x) (parse-integer (aref matches x))) '(1 3))))
      (#\p (cons 'partner (mapcar (lambda (x) (char (aref matches x) 0)) '(1 3)))))))

(defun exchange (programs a b)
  (let ((tmp (char programs a)))
    (setf (char programs a) (char programs b))
    (setf (char programs b) tmp)))

(defun dance (programs moves)
  (unless (null moves)
    (match (car moves)
      ((list 'spin n)
       (let ((copy-programs (copy-seq programs)))
         (loop for i below +nprogs+
               do (setf (char programs (rem (+ i n) +nprogs+)) (char copy-programs i)))))
      ((list 'xchange a b) (exchange programs a b))
      ((list 'partner a b) (exchange programs (position a programs) (position b programs)))
      (x (format t "detected: ~a~%" x)))
    (dance programs (cdr moves))))

(defun seen (programs moves map &optional (n 1))
  (match (lookup map programs)
    (nil 
      (let ((programs-copy (copy-seq programs)))
        (dance programs moves)
        (seen programs moves (with map programs-copy n) (1+ n))))
    (x (cons x (mapcar #'car (sort (convert 'list map) #'< :key #'cdr))))))

(defun main ()
  (let*
    ((moves (mapcar #'parse-move (split "," (car (read-input-as-list 16)))))
     (programs 
       (loop with str = (make-string +nprogs+)
             for i below +nprogs+
             do (setf (char str i) (code-char (+ (char-code #\a) i)))
             finally (return str)))
     (seen-map (with (empty-map) (copy-seq programs) 0)))
    (dance programs moves)
    (format t "~a~%"  programs)
    (destructuring-bind (n . lst) (seen programs moves seen-map)
      (format t "~a~%" (nth (+ n (rem 1000000000 (length lst))) lst)))))
