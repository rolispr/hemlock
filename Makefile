SBCL ?= sbcl
CC   ?= cc

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  SHLIB_EXT  := dylib
  SHLIB_FLAG := -dynamiclib
else
  SHLIB_EXT  := so
  SHLIB_FLAG := -shared -fPIC
endif

SBCL_INIT := --eval '(require :asdf)' \
             --eval '(when (probe-file "ocicl/ocicl-runtime.lisp") (load "ocicl/ocicl-runtime.lisp"))' \
             --eval '(asdf:initialize-source-registry (list :source-registry (list :directory (uiop:getcwd)) :inherit-configuration))'

.PHONY: build test clean

src/tty/libspawn-ctty.$(SHLIB_EXT): src/tty/spawn-ctty.c
	$(CC) $(SHLIB_FLAG) -o $@ $<

build: src/tty/libspawn-ctty.$(SHLIB_EXT)
	$(SBCL) --non-interactive $(SBCL_INIT) \
		--eval '(push :deploy-console *features*)' \
		--eval '(asdf:load-system :hemlock)' \
		--eval '(asdf:make :hemlock)'

test:
	$(SBCL) --non-interactive $(SBCL_INIT) \
		--eval '(asdf:test-system :hemlock-tests)'

clean:
	rm -rf bin/ src/tty/libspawn-ctty.so src/tty/libspawn-ctty.dylib
