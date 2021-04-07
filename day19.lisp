(defpackage :day19
  (:use :cl :aoc-misc :aoc-coord)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day19)


(defun parse-line (line)
  (nlet rec ((str (coerce line 'list)) (i 0) (lst nil))
    (if (null str)
      (reverse lst)
      (rec
        (cdr str)
        (1+ i)
        (if (char= (car str) #\Space) lst (cons (cons i (car str)) lst))))))

(defun find-start-coord (circuit)
  (nlet rec ((i 0))
    (if (not (char= (aref circuit 0 i) #\Space))
      (make-coord i 0)
      (rec (1+ i)))))

(defun get-char (circuit coord)
  (aref circuit (get-y coord) (get-x coord)))

(defun find-letters (circuit start-coord)
  (nlet rec ((coord start-coord) (direction 'south) (lst nil) (count 0))
    (let ((chr (get-char circuit coord)))
      (if (char= chr #\Space)
        (list (coerce (reverse lst) 'string) count)
        (cond
          ((and (char>= chr #\A) (char<= chr #\Z))
           (rec (next-coord direction coord) direction (cons chr lst) (1+ count)))
          ((char= chr #\+)
           (let 
             ((new-direction
                (turn 
                  (if (char= #\Space (get-char circuit (next-coord (turn 'left direction) coord))) 'right 'left) 
                  direction)))
             (rec (next-coord new-direction coord) new-direction lst (1+ count))))
          (t (rec (next-coord direction coord) direction lst (1+ count))))))))


(defun main ()
  (let*
    ((input (read-input-as-list 19 (lambda (str) (coerce str 'list))))
     (circuit (make-array (list (length input) (length (car input))) :initial-contents input))
     (start-coord (find-start-coord circuit)))
    (dolist (answer (find-letters circuit start-coord)) (format t "~a~%" answer))))
