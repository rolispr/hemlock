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
                  :trivial-garbage
                  :cl-ppcre
                  :command-line-arguments
                  :sento)
    :components
    ((:module wire
              :pathname "src/wire/"
              :depends-on ()
              :serial t
              :components
              ((:file "wire-package")
               (:file "port")
               (:file "introspect" :pathname "../introspect")
               (:file "wire")
               (:file "remote")))
     (:module core-1
              :pathname "src/"
              :depends-on (wire)
              :components
              ((:file "package")
               (:file "lispdep" :pathname "text/lispdep" :depends-on ("package"))
               (:file "decls" :pathname "text/decls" :depends-on ("package"))
               (:file "struct" :pathname "text/struct" :depends-on ("package"))
               (:file "char-attrs" :pathname "text/char-attrs" :depends-on ("package"))
               (:file "key-event" :pathname "text/key-event" :depends-on ("package" "char-attrs"))
               ))
     (:module bitmap-1
              :pathname "src/"
              :depends-on (core-1)
              :components
              ((:file "keysym-defs")))
     (:module tree-sitter
              :pathname "src/tree-sitter-cl/"
              :depends-on (core-1)
              :serial t
              :components
              ((:file "package")
               (:file "ffi")
               (:file "types")
               (:file "parser")
               (:file "node")
               (:file "query")
               (:file "language")))
     (:module core-2
              :pathname "src/command/"
              :depends-on (bitmap-1 core-1 tree-sitter)
              :serial t
              :components
              ((:file "rompsite")
               (:file "input")
               (:file "macros")
               (:file "line" :pathname "../text/line")
               (:file "ring" :pathname "../text/ring")
               (:file "text-primitives" :pathname "../text/text-primitives")
               (:file "buffer")
               (:file "vars")
               (:file "key-dispatch")
               (:file "syntax")
               (:file "text-ops" :pathname "../text/text-ops")
               (:file "text-insert" :pathname "../text/text-insert")
               (:file "text-delete" :pathname "../text/text-delete")
               (:file "files")
               (:file "search1" :pathname "../text/search1")
               (:file "search2" :pathname "../text/search2")
               (:file "table" :pathname "../text/table")
               (:file "winimage")
               (:file "window")
               (:file "screen")
               (:file "linimage")
               (:file "cursor")
               (:file "display")
               (:file "syntax-highlight")
               (:file "ts-highlight")
               (:file "connections")
               (:file "repl" :pathname "../user/repl" :depends-on ("macros" "rompsite" "connections"))))
     (:module io
              :pathname "src/io/"
              :depends-on (core-2)
              :components
              ((:file "ioconnections")))
     (:module ui
              :pathname "src/ui/"
              :depends-on (core-2 io)
              :serial t
              :components
              ((:file "nodes")
               (:file "render")
               (:file "input")))
     (:module root-1
              :pathname "src/command/"
              :depends-on (core-2 core-1 io ui)
              :components
              ((:file "font")
               (:file "streams")
               (:file "main" :pathname "../user/main")
               (:file "prepl" :pathname "../user/prepl" :depends-on ("main"))
               (:file "echo")
               (:file "undo-system")))
     (:module core-3
              :pathname "src/command/"
              :depends-on (bitmap-1 core-1 core-2)
              :components
              ((:file "typeout")))
     (:module user-1
              :pathname "src/user/"
              :depends-on (root-1 core-1 wire)
              :components
              ((:file "completion-source")
               (:file "echo-completion")
               (:file "echo-commands" :depends-on ("echo-completion"))
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
               (:file "spell-rt" :pathname "../spell/spell-rt")
               (:file "spell-corr" :pathname "../spell/spell-corr" :depends-on ("spell-rt"))
               (:file "spell-aug" :pathname "../spell/spell-aug" :depends-on ("spell-corr"))
               (:file "spell-commands" :depends-on ("spell-aug" "file-commands"))
               (:file "comments")
               (:file "overwrite")
               #+nil (:file "abbrev")
               (:file "italic-mode")
               (:file "char-syntax")
               (:file "edit-defs")
               (:file "auto-save")
               (:file "register")
               (:file "highlight")
               (:file "dired" :pathname "../dired/dired")
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
