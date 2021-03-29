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
  (when x (if (characterp x) (lookup registers x) x)))

(defun common-functions (prog-line i registers)
  (match prog-line
    ((list instr x y)
     (let
       ((val-x (get-value x registers))
        (val-y (get-value y registers)))
       (case instr
         (:|set| (cons (1+ i) (with registers x val-y)))
         (:|add| (cons (1+ i) (with registers x (+   val-x val-y))))
         (:|mul| (cons (1+ i) (with registers x (*   val-x val-y))))
         (:|mod| (cons (1+ i) (with registers x (mod val-x val-y))))
         (:|jgz| (cons (if (> val-x 0) (+ i val-y) (1+ i)) registers)))))))

(defun run-program (program)
  (nlet rec ((i 0) (last-note 0) (registers (empty-map 0)))
    (let ((prog-line (aref program i)))
      (match prog-line
        ((list instr x _)
         (case instr
           (:|snd| (rec (1+ i) (get-value x registers) registers))
           (:|rcv| (if (zerop (get-value x registers)) (rec (1+ i) last-note registers) last-note))
           (otherwise
             (match (common-functions prog-line i registers)
               ((cons new-i new-registers) (rec new-i last-note new-registers))))))))))

(defun main ()
  (let
    ((input (coerce (read-input-as-list 18 #'parse-line) 'vector)))
    (print (run-program input))))
