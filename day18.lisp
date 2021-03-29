(defpackage :day18
  (:use :cl :aoc-misc)
  (:import-from :cl-ppcre :split)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match)
  (:import-from :fset :empty-map :with :lookup)
  (:export main))

(in-package :day18)

(defun parse-line (line)
  (let ((tokens (split " " line)))
    (list
      (intern (first tokens) "KEYWORD")
      (char (second tokens) 0)
      (when (third tokens)
        (let ((first-char (char (third tokens) 0)))
          (if (and (char>= first-char #\a) (char<= first-char #\z))
            first-char
            (parse-integer (third tokens))))))))

(defun get-value (x registers)
  (if (characterp x) (lookup registers x) x))

(defun run-program (program)
  (nlet rec ((i 0) (last-note 0) (registers (empty-map 0)))
    (match (aref program i)
      ((list :|snd| freq nil)
       (rec (1+ i) (get-value freq registers) registers))
      ((list :|set| reg y)
       (rec (1+ i) last-note (with registers reg (get-value y registers))))
      ((list :|add| reg y)
       (rec (1+ i) last-note (with registers reg (+ (lookup registers reg) (get-value y registers)))))
      ((list :|mul| reg y)
       (rec (1+ i) last-note (with registers reg (* (lookup registers reg) (get-value y registers)))))
      ((list :|mod| reg y)
       (rec (1+ i) last-note (with registers reg (mod (lookup registers reg) (get-value y registers)))))
      ((list :|rcv| x nil)
       (if (zerop (get-value x registers)) (rec (1+ i) last-note registers) last-note))
      ((list :|jgz| x y)
       (rec (if (> (get-value x registers) 0) (+ i (get-value y registers)) (1+ i)) last-note registers)))))

(defun main ()
  (let
    ((input (read-input-as-list 18 #'parse-line)))
    (print (run-program (coerce input 'vector)))))
