#!/bin/bash

stop_on_failure="${1:-t}"
debug="${2:-nil}"
print="${3:-(quote t)}"
describe="${4:-(quote try:unexpected)}"
num_passes=
num_failures=

function run_test_case {
  local test_case_name="\"$1\""
  shift
  echo "SHTEST: Running ${test_case_name} $@"
  $@
  local retval=$?
  if ((retval == 22)); then
    echo
    echo "SHTEST: ${test_case_name} PASS"
    num_passes=$((num_passes+1))
  else
    echo
    echo "SHTEST: ${test_case_name} FAIL"
    num_failures=$((num_failures+1))
  fi
}

function lisp_tests {
  local lisp_name="$1"
  shift

  run_test_case "lisp test suite on ${lisp_name}" $@ <<EOF
(require :asdf)
(asdf:load-system :mgl-pax/test)
(when (try:passedp (mgl-pax-test:test :debug ${debug} :print ${print}
                                      :describe ${describe}))
  (uiop/image:quit 22))
EOF
}

function autoload_tests {
  local lisp_name="$1"
  shift

  run_test_case "test-locate-autoload on ${lisp_name}" $@ <<EOF
(asdf:load-system :mgl-pax/test-extension)
(in-package :mgl-pax-test-extension)
(when (passedp (try 'test-locate-autoload))
  (uiop/image:quit 22))
EOF

  run_test_case "test-canonical-reference-autoload on ${lisp_name}" $@ <<EOF
(asdf:load-system :mgl-pax/test-extension)
(in-package :mgl-pax-test-extension)
(when (passedp (try 'test-canonical-reference-autoload))
  (uiop/image:quit 22))
EOF

  run_test_case "test-collect-reachable-objects-autoload on ${lisp_name}" $@ <<EOF
(asdf:load-system :mgl-pax/test-extension)
(in-package :mgl-pax-test-extension)
(when (passedp (try 'test-collect-reachable-objects-autoload))
  (uiop/image:quit 22))
EOF

  run_test_case "test-document-autoload on ${lisp_name}" $@ <<EOF
(asdf:load-system :mgl-pax/test-extension)
(in-package :mgl-pax-test-extension)
(when (passedp (try 'test-document-autoload))
  (uiop/image:quit 22))
EOF

  run_test_case "test-docstring-autoload on ${lisp_name}" $@ <<EOF
(asdf:load-system :mgl-pax/test-extension)
(in-package :mgl-pax-test-extension)
(when (passedp (try 'test-docstring-autoload))
  (uiop/image:quit 22))
EOF

  run_test_case "test-find-source-autoload on ${lisp_name}" $@ <<EOF
(asdf:load-system :mgl-pax/test-extension)
(in-package :mgl-pax-test-extension)
(when (passedp (try 'test-find-source-autoload))
  (uiop/image:quit 22))
EOF
}

function basic_load_tests {
  local lisp_name="$1"
  shift

  run_test_case "load mgl-pax on ${lisp_name}" $@ <<EOF
(progn
  (asdf:load-system :mgl-pax)
  (uiop/image:quit 22))
EOF
}

function run_tests {
  local test_suite="$1"
  local lisp="$2"
  shift; shift
  echo
  echo "SHTEST: running test suite ${test_suite} with ${lisp} $@"
  num_failures=0
  num_passes=0
  if [ ${lisp} = "cmu-bin" ]; then
    # While loading ESRAP, some things are redefined that trigger warnings.
    ros --lisp "${lisp}" run --eval \
      '(handler-bind
           ((error (function continue)))
         (ql:quickload :mgl-pax/full))' \
      --quit -- $@
  else
    ros --lisp "${lisp}" run --eval '(ql:quickload :mgl-pax/full)' --quit -- $@
  fi
  ${test_suite} ${lisp} ros --lisp ${lisp} run -- $@
  if ((num_failures > 0)); then
    if [ ${stop_on_failure} = "t" ]; then
      echo "SHTEST: Aborting with ${num_failures} failures,"\
           "${num_passes} passes."
      exit 1
    fi
  fi
}

# Most lisps take only 10s or so to run the tests. CLISP takes 4x longer. ABCL
# is 25x slower.
run_tests lisp_tests sbcl --noinform --disable-debugger
run_tests lisp_tests allegro --batch --backtrace-on-error
run_tests lisp_tests ccl-bin --batch
run_tests lisp_tests cmu-bin -batch
run_tests lisp_tests ecl
run_tests lisp_tests clisp -on-error exit
run_tests lisp_tests abcl-bin
# We run the autoload tests on the faster ones only.
run_tests autoload_tests sbcl --noinform --disable-debugger
run_tests autoload_tests allegro --batch --backtrace-on-error
run_tests autoload_tests ccl-bin --batch
run_tests autoload_tests cmu-bin -batch
run_tests autoload_tests ecl

echo "SHTEST: ${num_failures} failures, ${num_passes} passes."
