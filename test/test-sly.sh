#!/bin/bash

# Run the Lisp-side tests of the Sly support (test/test-sly.lisp) and of the interaction mode
# protocol (test/test-interaction-mode.lisp, which is part of the main MGL-PAX-TEST suite too).
# These tests don't need Emacs or a running Slynk server, but Slynk has to be loadable.
#
# Usage: test-sly.sh
#
# Environment:
#   LISP       the Lisp to run (default: sbcl)
#   SLYNK_DIR  directory containing slynk-loader.lisp (default: the
#              slynk directory of the newest sly package under
#              ~/.emacs.d/elpa/)

cd $(dirname $0)/..
ROOT=$(pwd)
LISP=${LISP:-sbcl}
SLYNK_DIR=${SLYNK_DIR:-$(ls -d ~/.emacs.d/elpa/sly-*/slynk 2>/dev/null | tail -n 1)}

if [ ! -f "${SLYNK_DIR}/slynk-loader.lisp" ]; then
        echo "Cannot find slynk-loader.lisp. Set SLYNK_DIR." >&2
        exit 2
fi

${LISP} --noinform --disable-debugger <<EOF
(require :asdf)
(push #p"${ROOT}/" asdf:*central-registry*)
(push #p"${ROOT}/dref/" asdf:*central-registry*)
(push #p"${ROOT}/test/" asdf:*central-registry*)
(load "${SLYNK_DIR}/slynk-loader.lisp")
(slynk-loader:init)
(ql:quickload :mgl-pax-test :silent t)
(asdf:load-system "mgl-pax/sly")
(in-package :mgl-pax-test)
(load "${ROOT}/test/test-sly.lisp")
(let ((*package* (find-package :common-lisp))
      (*print-duration* nil)
      (ok t))
  (dolist (test '(test-interaction-mode test-sly))
    (let ((trial (try test :print 'unexpected :describe 'unexpected)))
      (format t "~&~S: ~:[FAILED~;passed~]~%" test (passedp trial))
      (unless (passedp trial)
        (setq ok nil))))
  (uiop:quit (if ok 0 1)))
EOF
