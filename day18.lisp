(defpackage :day18
  (:use :cl :aoc-misc :functional-queue)
  (:import-from :cl-ppcre :split)
  (:import-from :serapeum :nlet)
  (:import-from :trivia :match)
  (:import-from :fset :empty-map :with :lookup)
  (:export main))

(in-package :day18)

(defun reg-or-int (str)
  (when str
    (let ((first-char (char str 0)))
      (if (and (char>= first-char #\a) (char<= first-char #\z))
        first-char
        (parse-integer str)))))

(defun parse-line (line)
  (let ((tokens (split " " line)))
    (list
      (intern (first  tokens) "KEYWORD")
      (reg-or-int (second tokens))
      (reg-or-int (third  tokens)))))

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

(defun run-program2 (program)
  (nlet rec ((state       (list 0 0 (with (empty-map 0) #\p 0)))
             (state-other (list 1 0 (with (empty-map 0) #\p 1)))
             (queue-input  (empty-queue))
             (queue-output (empty-queue))
             (count 0))
    (match state
      ((list id ptr registers)
       (let ((prog-line (aref program ptr)))
         (match prog-line
           ((list instr x _)
            (case instr
              (:|snd|
                (rec
                  (list id (1+ ptr) registers)
                  state-other
                  queue-input
                  (queue-snoc queue-output (get-value x registers))
                  (if (= 1 id) (1+ count) count)))
              (:|rcv|
                (if (queue-empty-p queue-input)
                  (if (queue-empty-p queue-output)
                    count
                    (rec state-other state queue-output queue-input count))
                  (rec 
                    (list id (1+ ptr) (with registers x (queue-head queue-input)))
                    state-other
                    (queue-tail queue-input)
                    queue-output
                    count)))
              (otherwise
                (match (common-functions prog-line ptr registers)
                  ((cons new-ptr new-registers)
                   (rec
                     (list id new-ptr new-registers)
                     state-other
                     queue-input
                     queue-output
                     count))))))))))))

(defun main ()
  (let
    ((program (coerce (read-input-as-list 18 #'parse-line) 'vector)))
    (print (run-program  program))
    (print (run-program2 program))))

