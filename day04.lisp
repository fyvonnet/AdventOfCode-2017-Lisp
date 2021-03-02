(defpackage :day04
  (:use :cl :aoc-misc :cl-ppcre)
  (:import-from :serapeum :nlet)
  (:export main))

(in-package :day04)


(defun valid-passphrase-p (passphrase)
  (nlet rec ((lst (sort (copy-list passphrase) #'string<)))
    (cond 
      ((null (cdr lst)) t)
      ((string= (first lst) (second lst)) nil)
      (t (rec (cdr lst))))))

(defun count-valid (passphrases)
  (length (delete-if-not #'valid-passphrase-p passphrases)))

(defun main ()
  (let
    ((input (mapcar (lambda (s) (split " " s)) (read-input-as-list 4))))
    (print (count-valid input))
    (loop
      for passphrase in input
      do (loop
           for word in passphrase
           do (sort word #'char<)))
    (print (count-valid input))))
