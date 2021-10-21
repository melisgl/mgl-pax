#!/bin/bash

silent="${1:-nil}"
stop_on_failure="${2:-t}"
update_baseline="${3:-nil}"
num_passes=
num_failures=

function run_test_case {
  local test_case_name="\"$1\""
  shift
  echo "SHTEST: Running ${test_case_name} $@"
  $@
  local retval=$?
  if ((retval == 22)); then
    echo "SHTEST: ${test_case_name} PASSED"
    num_passes=$((num_passes+1))
  else
    echo "SHTEST: ${test_case_name} FAILED"
    num_failures=$((num_failures+1))
  fi
}

function lisp_tests {
  local lisp_name="$1"
  shift

  run_test_case "lisp test suite on ${lisp_name}" $@ <<EOF
(require :asdf)
(asdf:load-system :mgl-pax/test)
(setq mgl-pax-test::*update-baseline* ${update_baseline})
(progn
  (asdf:test-system :mgl-pax)
  (uiop/image:quit 22))
EOF
}

function autoload_tests {
  local lisp_name="$1"
  shift

  run_test_case "load mgl-pax/navigate on ${lisp_name}" $@ <<EOF
(progn
  (asdf:load-system :mgl-pax/navigate)
  (uiop/image:quit 22))
EOF

  run_test_case "load mgl-pax/navigate on ${lisp_name}" $@ <<EOF
(progn
  (asdf:load-system :mgl-pax/document)
  (uiop/image:quit 22))
EOF

  run_test_case "load mgl-pax/transcribe on ${lisp_name}" $@ <<EOF
(progn
  (asdf:load-system :mgl-pax/transcribe)
  (uiop/image:quit 22))
EOF

  run_test_case "autoload mgl-pax/navigate on ${lisp_name}" $@ <<EOF
(asdf:load-system :mgl-pax)
(progn
  (mgl-pax:locate-definitions-for-emacs "mgl-pax:section" "class")
  (uiop/image:quit 22))
EOF

  run_test_case "autoload mgl-pax/document on ${lisp_name}" $@ <<EOF
(asdf:load-system :mgl-pax)
(progn
  (defun foo ()
    "docstring"
    nil)
  (mgl-pax:document #'foo)
  (uiop/image:quit 22))
EOF

  run_test_case "autoload mgl-pax/transcribe on ${lisp_name}" $@ <<EOF
(asdf:load-system :mgl-pax)
(progn
  (mgl-pax:transcribe "*print-level*" nil)
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
  if [ "${silent}" != "nil" ]; then
    ${test_suite} ${lisp} ros --lisp ${lisp} run -- $@ \
      | grep --line-buffered "SHTEST:"
  else
    ${test_suite} ${lisp} ros --lisp ${lisp} run -- $@
  fi
  echo "SHTEST: ${lisp}: ${num_failures} failures, ${num_passes} passes."
  if ((num_failures > 0)); then
    if [ $stop_on_failure ]; then
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
