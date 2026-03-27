;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defparameter *hemlock-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(asdf:defsystem :hemlock.webui
  :description "WebUI display backend for Hemlock"
  :licence "Public Domain"
  :depends-on (:hemlock :cl-webui :sb-concurrency)
  :pathname #.(make-pathname
               :directory
               (pathname-directory *hemlock-base-directory*)
               :defaults *hemlock-base-directory*)
  :components
  ((:module webui-1
    :pathname "src/webui/"
    :serial t
    :components
    ((:file "device")
     (:file "screen")
     (:file "input")
     (:file "display")))))
