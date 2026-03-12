;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defparameter *hemlock-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(asdf:defsystem :hemlock.clx
     :pathname #.(make-pathname
                        :directory
                        (pathname-directory *hemlock-base-directory*)
                        :defaults *hemlock-base-directory*)
     :depends-on (:hemlock.base :clx)
    :components
    ((:module clx-1
              :pathname "src/clx/"
              :components
              ((:file "object-set")
               (:file "device")
               (:file "drawing" :depends-on ("device"))
               (:file "site")
               (:file "input")
               (:file "display" :depends-on ("drawing"))
               (:file "screen")
               (:file "events")
               (:file "typeout")))))
