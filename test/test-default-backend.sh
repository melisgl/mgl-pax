#!/bin/bash

# Check which backend (mgl-pax-slime or mgl-pax-sly) ends up loaded in Emacs
# for various order of loading mgl-pax and the clients.
# Environment variables: Environment: SLIME_DIR, SLY_DIR, W3M_DIR as in test-el.sh.

cd `dirname $0`
EMACS=${EMACS:-emacs}

newest() {
    ls -d ~/.emacs.d/elpa/"$1"-[0-9]* 2>/dev/null | sort -V | tail -n 1
}
SLIME_DIR=${SLIME_DIR:-$(newest slime)}
SLY_DIR=${SLY_DIR:-$(newest sly)}
W3M_DIR=${W3M_DIR:-$(newest w3m)}
for dir in "${SLIME_DIR}" "${SLY_DIR}"; do
    if [ ! -d "${dir}" ]; then
        echo "Cannot find both SLIME and SLY. Set SLIME_DIR and SLY_DIR." >&2
        exit 2
    fi
done

failed=0

# check DESCRIPTION EXPECTED-BACKEND FORM
# Evaluates FORM in a fresh Emacs and compares the value of
# `mgl-pax-backend' (or nil if unbound) with EXPECTED-BACKEND.
check() {
    local description=$1
    local expected=$2
    local form=$3
    local actual
    actual=$(${EMACS} -q --batch -L ../src/ -L "${SLIME_DIR}" -L "${SLY_DIR}" \
                      ${W3M_DIR:+-L "$W3M_DIR"} \
                      --eval "(progn ${form} (princ (format \"%S\" (and (boundp 'mgl-pax-backend) mgl-pax-backend))))" 2>&1 | tail -n 1)
    if [ "${actual}" = "${expected}" ]; then
        echo "ok   ${description}: ${actual}"
    else
        echo "FAIL ${description}: expected ${expected}, got ${actual}" >&2
        failed=1
    fi
}

check "require mgl-pax, no client" nil "(require 'mgl-pax)"
check "SLIME loaded, then mgl-pax" :slime "(progn (require 'slime) (require 'mgl-pax))"
check "mgl-pax, then SLIME" :slime "(progn (require 'mgl-pax) (require 'slime))"
check "Sly loaded, then mgl-pax" :sly "(progn (require 'sly) (require 'mgl-pax))"
check "mgl-pax, then Sly" :sly "(progn (require 'mgl-pax) (require 'sly))"
check "SLIME, then Sly, then mgl-pax: SLIME" :slime "(progn (require 'slime) (require 'sly) (require 'mgl-pax))"
check "Sly, then SLIME, then mgl-pax: SLIME anyway" :slime "(progn (require 'sly) (require 'slime) (require 'mgl-pax))"
check "mgl-pax, then Sly, then SLIME: Sly, the first loaded" :sly "(progn (require 'mgl-pax) (require 'sly) (require 'slime))"
check "explicit mgl-pax-slime" :slime "(require 'mgl-pax-slime)"
check "explicit mgl-pax-sly" :sly "(require 'mgl-pax-sly)"
check "explicit mgl-pax-sly, then SLIME loads" :sly "(progn (require 'mgl-pax-sly) (require 'slime))"
check "explicit mgl-pax-slime, then Sly loads" :slime "(progn (require 'mgl-pax-slime) (require 'sly))"

exit ${failed}
