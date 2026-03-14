;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defparameter *hemlock-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(asdf:defsystem :hemlock.tty
     :pathname #.(make-pathname
                        :directory
                        (pathname-directory *hemlock-base-directory*)
                        :defaults *hemlock-base-directory*)
     :depends-on (:hemlock.base)
    :components
    ((:module tty-1
              :pathname "src/tty/"
              :components
              ((:file "ioconnections")
               (:file "render")
               (:file "terminfo")
               (:file "termcap" :depends-on ("terminfo"))
               (:file "output" :depends-on ("render"))
               (:file "display" :depends-on ("terminfo" "output"))
               (:file "screen" :depends-on ("terminfo" "output"))
               (:file "device")
               (:file "input" :depends-on ("terminfo" "render" "screen"))
               (:file "linedit" :depends-on ("display"))))))
