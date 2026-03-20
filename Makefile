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

src/io/libspawn-ctty.$(SHLIB_EXT): src/io/spawn-ctty.c
	$(CC) $(SHLIB_FLAG) -o $@ $<

src/tree-sitter-cl/lib/libts-wrapper.$(SHLIB_EXT): src/tree-sitter-cl/c-wrapper/ts-wrapper.c
	$(CC) $(SHLIB_FLAG) -o $@ $< -ltree-sitter

build: src/io/libspawn-ctty.$(SHLIB_EXT) src/tree-sitter-cl/lib/libts-wrapper.$(SHLIB_EXT)
	$(SBCL) --non-interactive --load init.lisp \
		--eval '(push :deploy-console *features*)' \
		--eval '(asdf:load-system :hemlock)' \
		--eval '(defmethod deploy:output-file ((op deploy:deploy-op)) (merge-pathnames "bin/hemlock" (uiop:getcwd)))' \
		--eval '(asdf:make :hemlock)'

test: src/io/libspawn-ctty.$(SHLIB_EXT)
	$(SBCL) --non-interactive --load init.lisp \
		--eval '(asdf:test-system :hemlock-tests)'

clean:
	rm -rf bin/ src/io/libspawn-ctty.so src/io/libspawn-ctty.dylib \
	       src/tree-sitter-cl/lib/libts-wrapper.so src/tree-sitter-cl/lib/libts-wrapper.dylib
