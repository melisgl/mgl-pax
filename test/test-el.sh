#!/bin/bash

set -e

cd $(dirname $0)
EMACS=emacs
LISP="${1:-sbcl}"
# This is where the Github action expects to find SLIME. For local testing,
# we also look for it where elpa would install it for Emacs.
SLIME_DIR=~/.roswell/local-projects/slime/
if [ ! -d "${SLIME_DIR}" ]; then
        SLIME_DIR=$(ls -d ~/.emacs.d/elpa/slime-[0-9]* 2>/dev/null | sort -V | tail -n 1)
fi
if [ ! -d "${SLIME_DIR}" ]; then
        echo "Cannot find SLIME. Install it in ~/.roswell/local-projects/slime or ~/.emacs.d/elpa/." >&2
        exit 2
fi
W3M_DIR=${W3M_DIR:-$(ls -d ~/.emacs.d/elpa/w3m-* 2>/dev/null | sort -V | tail -n 1)}
LOAD_PATH="-L ../src/ -L . -L ${SLIME_DIR}"
if [ -n "${W3M_DIR}" ]; then
        LOAD_PATH="${LOAD_PATH} -L ${W3M_DIR}"
fi
SELECTOR=${2:-"\"mgl-pax\""}
# Randomization is a poor man's autoload test.
RANDOMIZE=t

${EMACS} -q --no-splash --batch ${LOAD_PATH} \
        --eval "(require 'mgl-pax-slime)" \
        --eval "(require 'mgl-pax-tests)" \
        --eval "(slime-setup)" \
        --eval "(setq inferior-lisp-program \"${LISP}\")" \
        --eval "(slime-batch-test (quote ${SELECTOR}) ${RANDOMIZE})"
