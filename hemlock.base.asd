;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:hemlock-system
  (:use #:cl)
  (:export #:*hemlock-base-directory*))

(in-package #:hemlock-system)

(pushnew :command-bits *features*)

(defparameter *hemlock-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(asdf:defsystem :hemlock.base
     :pathname #.(make-pathname
                        :directory
                        (pathname-directory *hemlock-base-directory*)
                        :defaults *hemlock-base-directory*)
     :depends-on (:alexandria
                  :bordeaux-threads
                  :trivial-gray-streams
                  :sb-bsd-sockets
                  :cffi
                  :babel
                  :cl-ppcre
                  :command-line-arguments)
    :components
    ((:module wire
              :pathname "src/"
              :depends-on ()
              :serial t
              :components
              ((:file "wire-package")
               (:file "port")
               (:file "introspect")
               (:file "wire")
               (:file "remote")))
     (:module core-1
              :pathname "src/"
              :depends-on (wire)
              :components
              ((:file "package")
               (:file "lispdep" :depends-on ("package"))
               (:file "hemlock-ext" :depends-on ("package"))
               (:file "decls" :depends-on ("package"))
               (:file "struct" :depends-on ("package"))
               (:file "char-attrs" :depends-on ("package"))
               (:file "key-event" :depends-on ("package" "char-attrs"))
               ))
     (:module bitmap-1
              :pathname "src/"
              :depends-on (core-1)
              :components
              ((:file "keysym-defs")))
     (:module core-2
              :pathname "src/"
              :depends-on (bitmap-1 core-1)
              :serial t
              :components
              ((:file "rompsite")
               (:file "input")
               (:file "macros")
               (:file "line")
               (:file "ring")
               (:file "text-primitives")
               (:file "buffer")
               (:file "vars")
               (:file "key-dispatch")
               (:file "syntax")
               (:file "text-ops")
               (:file "text-insert")
               (:file "text-delete")
               (:file "files")
               (:file "search1")
               (:file "search2")
               (:file "table")
               (:file "winimage")
               (:file "window")
               (:file "screen")
               (:file "linimage")
               (:file "cursor")
               (:file "display")
               (:file "syntax-highlight")
               (:file "connections")
               (:file "repl" :depends-on ("macros" "rompsite" "connections"))))
     (:module ioconn
              :pathname "src/tty/"
              :depends-on (core-2 root-1)
              :components
              ((:file "ioconnections")))
     (:module root-1
              :pathname "src/"
              :depends-on (core-2 core-1)
              :components
              ((:file "pop-up-stream")))
     (:module root-2
              :pathname "src/"
              :depends-on (root-1 core-1 wire)
              :components
              ((:file "font")
               (:file "streams")
               (:file "main")
               (:file "prepl" :depends-on ("main"))
               (:file "echo")
               (:file "undo-system")))
     (:module core-3
              :pathname "src/"
              :depends-on (bitmap-1 core-1 core-2)
              :components
              ((:file "typeout")))
     (:module user-1
              :pathname "src/"
              :depends-on (root-2 core-1 wire)
              :components
              ((:file "completion-source")
               (:file "echo-commands")
               (:file "command")
               (:file "kbd-macro")
               (:file "undo")
               (:file "kill-commands")
               (:file "indent" :depends-on ("file-commands"))
               (:file "search-commands")
               (:file "file-commands")
               (:file "grep" :depends-on ("file-commands"))
               (:file "apropos" :depends-on ("file-commands"))
               (:file "more-commands")
               (:file "help-commands")
               (:file "source-compare")
               (:file "group")
               (:file "fill")
               (:file "text")
               (:file "lispmode")
               (:file "repl-buf")
               (:file "repl-stream")
               (:file "request")
               (:file "eval-server")
               (:file "lisp-commands" :depends-on ("file-commands"))
               (:file "lispeval" :depends-on ("eval-server"))
               (:file "spell-rt")
               (:file "spell-corr" :depends-on ("spell-rt"))
               (:file "spell-aug" :depends-on ("spell-corr"))
               (:file "spell-commands" :depends-on ("spell-aug" "file-commands"))
               (:file "comments")
               (:file "overwrite")
               #+nil (:file "abbrev")
               (:file "italic-mode")
               (:file "char-syntax")
               (:file "edit-defs")
               (:file "auto-save")
               (:file "register")
               (:file "x-commands")
               (:file "highlight")
               (:file "dired")
               (:file "dired-commands" :depends-on ("dired"))
               (:file "buflist")
               (:file "connections-ui")
               (:file "xref")
               (:file "completion" :depends-on ("lispmode"))
               (:file "symbol-completion")
               (:file "fuzzy" :depends-on ("symbol-completion"))
               (:file "shell")
               (:file "debug")
               (:file "dabbrev")
               (:file "bindings")))))
