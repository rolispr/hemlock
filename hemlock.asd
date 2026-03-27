;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:hemlock-system
  (:use #:cl)
  (:export #:*hemlock-base-directory*))

(in-package #:hemlock-system)

(pushnew :command-bits *features*)

(defparameter *hemlock-base-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(asdf:defsystem :hemlock
     :description "Hemlock text editor"
     :licence "Public Domain"
     :defsystem-depends-on (:deploy)
     :build-operation "deploy-op"
     :build-pathname "hemlock"
     :entry-point "hemlock::main"
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
                  :sento
                  :sento-remoting
                  :fset)
    :components
    (;; Wire protocol
     (:module wire
              :pathname "src/wire/"
              :serial t
              :components
              ((:file "wire-package")
               (:file "port")
               (:file "introspect" :pathname "../introspect")
               (:file "wire")
               (:file "remote")))

     ;; Packages, types, structs
     (:module core-1
              :pathname "src/"
              :depends-on (wire)
              :components
              ((:file "package")
               (:file "lispdep" :pathname "text/lispdep" :depends-on ("package"))
               (:file "decls" :pathname "text/decls" :depends-on ("package"))
               (:file "struct" :pathname "text/struct" :depends-on ("package"))
               (:file "char-attrs" :pathname "text/char-attrs" :depends-on ("package"))
               (:file "key-event" :pathname "text/key-event" :depends-on ("package" "char-attrs"))))

     ;; Actor system
     (:module actor
              :pathname "src/actor/"
              :depends-on (core-1)
              :serial t
              :components
              ((:file "system")
               (:file "registry")
               (:file "local-agent")
               (:file "spawn")))

     ;; Keysyms
     (:module bitmap-1
              :pathname "src/"
              :depends-on (core-1)
              :components
              ((:file "keysym-defs")))

     ;; Tree-sitter
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

     ;; Command framework, text model, display
     (:module core-2
              :pathname "src/command/"
              :depends-on (bitmap-1 core-1 tree-sitter)
              :serial t
              :components
              ((:file "platform")
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
               (:file "fset-state" :pathname "../text/fset-state")
               (:file "fset-shadow")
               (:file "winimage")
               (:file "window")
               (:file "screen")
               (:file "linimage")
               (:file "cursor")
               (:file "display")
               (:file "syntax-highlight")
               (:file "palette")
               (:file "ts-highlight")
               (:file "connections")
               (:file "repl" :pathname "../user/repl" :depends-on ("macros" "platform" "connections"))))

     ;; I/O (serve-event bridge)
     (:module io
              :pathname "src/io/"
              :depends-on (core-2)
              :components
              ((:file "ioconnections")))

     ;; UI tree
     (:module ui
              :pathname "src/ui/"
              :depends-on (core-2 io)
              :serial t
              :components
              ((:file "nodes")
               (:file "render")
               (:file "input")))

     ;; Editor entry, streams, echo
     (:module editor
              :pathname "src/command/"
              :depends-on (core-2 core-1 io ui)
              :components
              ((:file "font")
               (:file "streams")
               (:file "main" :pathname "../user/main")
               (:file "prepl" :pathname "../user/prepl" :depends-on ("main"))
               (:file "echo")
               (:file "undo-system")))

     ;; Typeout
     (:module typeout
              :pathname "src/command/"
              :depends-on (bitmap-1 core-1 core-2)
              :components
              ((:file "typeout")))

     ;; User commands
     (:module commands
              :pathname "src/user/"
              :depends-on (editor core-1 wire)
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
               (:file "complete-symbol" :depends-on ("symbol-completion"))
               (:file "shell")
               (:file "debug")
               (:file "dabbrev")
               (:file "modal")
               (:file "modal-textobjects" :depends-on ("modal"))
               (:file "modal-bindings"    :depends-on ("modal" "modal-textobjects"))
               (:file "bindings")))

     ;; TTY backend
     (:module tty
              :pathname "src/tty/"
              :depends-on (core-2 editor io commands)
              :components
              ((:file "render")
               (:file "terminfo")
               (:file "termcap" :depends-on ("terminfo"))
               (:file "output" :depends-on ("render"))
               (:file "display" :depends-on ("terminfo" "output"))
               (:file "screen" :depends-on ("terminfo" "output"))
               (:file "device")
               (:file "input" :depends-on ("terminfo" "render" "screen"))
               (:file "linedit" :depends-on ("display"))))

     ;; Terminal emulator
     (:module term
              :pathname "src/term/"
              :depends-on (core-2 editor commands)
              :serial t
              :components
              ((:file "package")
               (:file "types")
               (:file "color")
               (:file "sgr")
               (:file "ops")
               (:file "write")
               (:file "parser")
               (:file "pty")
               (:file "input")
               (:file "render")
               (:file "mode")))))


;;; For building binaries
(deploy:define-hook (:deploy asdf) (directory)
  (declare (ignorable directory))
  #+asdf (asdf:clear-source-registry)
  #+asdf (defun asdf:upgrade-asdf () nil))
