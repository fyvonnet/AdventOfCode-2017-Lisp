(defsystem "adventofcode-2017"
           :description "Advent of Code 2017 in LISP"
           :author "Franck YVONNET"
           :serial t
           :depends-on (:aoc-misc
                        :aoc-coord
                        :cl-ppcre
                        :fset
                        :functional-queue
                        :serapeum)
           :components ((:file "knot-hash")
                        (:file "day01")
                        (:file "day02")
                        (:file "day03")
                        (:file "day04")
                        (:file "day05")
                        (:file "day06")
                        (:file "day07")
                        (:file "day08")
                        (:file "day09")
                        (:file "day10")
                        (:file "day11")
                        (:file "day12")
                        (:file "day13")
                        (:file "day14")
                        (:file "day15")
                        (:file "day16")
                        (:file "day17")
                        (:file "day18")
                        (:file "day19")
                        (:file "day20")
                        (:file "day21")
                        (:file "day22")
                        (:file "day23")
                        (:file "day24")))
