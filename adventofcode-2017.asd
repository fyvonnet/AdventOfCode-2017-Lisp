(defsystem "adventofcode-2017"
           :description "Advent of Code 2017 in LISP"
           :author "Franck YVONNET"
           :serial t
           :depends-on (:aoc-misc
                        :aoc-coord
                        :cl-ppcre
                        :fset
                        :serapeum)
           :components ((:file "day01")
                        (:file "day02")
                        (:file "day03")
                        (:file "day04")
                        (:file "day05")
                        (:file "day06")
                        (:file "day07")))