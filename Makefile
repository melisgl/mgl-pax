.PHONY: test-full test-lisps test-el test-sbcl-common-lisp update-docs


MAKEFILE_DIR := $(dir $(lastword $(MAKEFILE_LIST)))

ifneq ($(wildcard $(MAKEFILE_DIR).qlot),)
	SBCL := qlot exec sbcl --noinform --non-interactive --eval '(require :asdf)' --eval
else
SBCL := sbcl --noinform --non-interactive --eval '(require :asdf)' --eval

endif

test-full: test-lisps test-el test-default-backend

test-lisps: ./test/test.sh
	./test/test.sh

test-el: ./test/test-el.sh
	./test/test-el.sh

test-default-backend: ./test/test-default-backend.sh
	./test/test-default-backend.sh

test-sbcl-common-lisp:
	@echo "Running Common Lisp tests"
	$(SBCL) '(let ((*compile-verbose* nil) (*compile-print* nil)) (asdf:test-system "mgl-pax"))'
	$(SBCL) '(let ((*compile-verbose* nil) (*compile-print* nil)) (asdf:test-system "dref"))'

update-docs:
	sbcl --noinform --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "mgl-pax.asd"))' \
		--eval '(asdf:load-system "mgl-pax/full")' \
		--eval '(handler-bind ((pax::transcription-error (function continue))) (pax::update-pax-readmes))'
