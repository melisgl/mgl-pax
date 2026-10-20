#!/bin/bash

# Run the Elisp tests with each Lisp client in turn.
#
# Usage: test-el.sh [LISP [SELECTOR]]
#
# - MGL_PAX_TEST_CLIENTS: space separated list of clients (default: "slime sly")

set -e

cd $(dirname $0)
EMACS=emacs
LISP="${1:-sbcl}"
CLIENTS=("slime" "sly")
if [ -n "${MGL_PAX_TEST_CLIENTS}" ]; then
    CLIENTS=(${MGL_PAX_TEST_CLIENTS})
fi
CLIENT_DIRS=()
ROS_LOAD_PATH=~/.roswell/local-projects/

# The Github test action expects to find SLIME in
# ~/.roswell/local-projects/. For local testing, we also look for it
# where elpa would install it for Emacs. The same should work for Sly.
# Hypothetically, if you needed to add another client that didn't fit
# this pattern, it would be easiest to exclude it from CLIENTS and add
# it to CLIENT_DIRS after this loop.

newest() {
    ls -d ~/.emacs.d/elpa/"$1"-[0-9]* 2>/dev/null | sort -V | tail -n 1
}
LOAD_PATH="-L ../src/ -L ."
for client in "${CLIENTS[@]}"; do
    client_path="${ROS_LOAD_PATH}/${client}"
    if [ ! -d "${client_path}" ]; then
        client_path="$(newest "${client}")"
    fi
    CLIENT_DIRS+=("${client_path}")
    if [ -d "${client_path}" ]; then
        LOAD_PATH+=" -L ${client_path} -L ${client_path}/lib"
    fi
done


W3M_DIR=${W3M_DIR:-$(ls -d ~/.emacs.d/elpa/w3m-* 2>/dev/null | sort -V | tail -n 1)}

if [ -n "${W3M_DIR}" ]; then
        LOAD_PATH="${LOAD_PATH} -L ${W3M_DIR}"
fi
SELECTOR=${2:-"\"mgl-pax\""}
# Randomization is a poor man's autoload test.
RANDOMIZE=t

clients_found=0
failed=0

for i in "${!CLIENT_DIRS[@]}"; do
    client_dir="${CLIENT_DIRS[$i]}"
    client="${CLIENTS[$i]}"
    if [ ! -d "${client_dir}" ]; then
            echo "Cannot find ${client}. Install the ${client} in ~/.roswell/local-projects/ or ~/.emacs.d/elpa/." >&2
            continue
    fi
    clients_found=$((clients_found + 1))
    echo "Testing Emacs with ${client}."
    set +e
    ${EMACS} -q --no-splash --batch ${LOAD_PATH} \
            --eval "(setq mgl-pax-test-client '${client})" \
            --eval "(setq mgl-pax-backend :${client})" \
            --eval "(require '${client})" \
            --eval "(require 'mgl-pax)" \
            --eval "(setq inferior-lisp-program \"${LISP}\")" \
            --eval "(${client}-setup)" \
            --eval "(require 'mgl-pax-tests)" \
            --eval "(mgl-pax-test-batch-test (quote ${SELECTOR}) ${RANDOMIZE})"
    rc=$?
    set -e
    if [ ${rc} -ne 0 ]; then
        echo "Tests for ${client} failed." >&2
        failed=1
    fi
done

if [ "$clients_found" -eq 0 ]; then
    echo "Error: no Common Lisp clients found." >&2
    exit 2
fi

exit ${failed}
