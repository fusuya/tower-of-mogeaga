(ql:quickload :ftw)

(defpackage moge
  (:use #:cl #:ftw #:cffi))

(in-package moge)

(load "item.lisp" :external-format :utf-8)
(load "define.lisp" :external-format :utf-8)
(load "maze-test.lisp" :external-format :utf-8)
;;(load "make-monster.lisp" :external-format :utf-8)
(load "tou-moge.lisp" :external-format :utf-8)


#|
(sb-ext:save-lisp-and-die "mogerpg"
        :toplevel #'main
        :save-runtime-options t
        :executable t)
|#
