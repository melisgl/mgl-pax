#!/bin/bash

cd `dirname $0`
EMACS=emacs
LISP=sbcl
# FIXME: This is hardcoded.
SLIME_DIR=~/src/slime/
LOAD_PATH="-L ../src/ -L . -L ${SLIME_DIR}"
SELECTOR="\"mgl-pax\""

${EMACS} -q --no-splash --batch ${LOAD_PATH} \
         --eval "(require 'mgl-pax-tests)" \
	 --eval "(slime-setup)" \
	 --eval "(setq inferior-lisp-program \"${LISP}\")" \
	 --eval "(slime-batch-test (quote ${SELECTOR}))"
