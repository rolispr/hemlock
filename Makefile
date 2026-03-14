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

.PHONY: build test clean

src/tty/libspawn-ctty.$(SHLIB_EXT): src/tty/spawn-ctty.c
	$(CC) $(SHLIB_FLAG) -o $@ $<

build: src/tty/libspawn-ctty.$(SHLIB_EXT)
	$(SBCL) --non-interactive --load init.lisp \
		--eval '(push :deploy-console *features*)' \
		--eval '(asdf:load-system :hemlock)' \
		--eval '(defmethod deploy:output-file ((op deploy:deploy-op)) (merge-pathnames "bin/hemlock" (uiop:getcwd)))' \
		--eval '(asdf:make :hemlock)'

test: src/tty/libspawn-ctty.$(SHLIB_EXT)
	$(SBCL) --non-interactive --load init.lisp \
		--eval '(asdf:test-system :hemlock-tests)'

clean:
	rm -rf bin/ src/tty/libspawn-ctty.so src/tty/libspawn-ctty.dylib
