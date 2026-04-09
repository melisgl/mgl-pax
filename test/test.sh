#!/bin/bash

lisp="$1"
stop_on_failure="${2:-t}"
debug="${3:-nil}"
print="${4:-(quote try:leaf)}"
describe="${5:-(quote (or try:unexpected try:failure))}"
num_passes=
num_failures=

function run_test_case {
  local test_case_name="\"$1\""
  shift
  echo "SHTEST: Running ${test_case_name} $@"
  "$@"
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

  run_test_case "lisp test suite on ${lisp_name}" "$@" <<EOF
(require :asdf)
(asdf:load-system :mgl-pax-test)
(when (try:passedp (mgl-pax-test:test :debug ${debug} :print ${print}
                                      :describe ${describe}))
  (uiop:quit 22))
EOF
}

function run_tests {
  local test_suite="$1"
  local failure_expected="$2"
  local lisp="$3"
  shift; shift; shift
  echo
  echo "SHTEST: Running test suite ${test_suite} with ${lisp} $@"
  num_failures=0
  num_passes=0
  if [ ${lisp} = "cmu-bin" ]; then
    # While loading ESRAP, some things are redefined that trigger warnings.
    ros --lisp "${lisp}" run --eval \
      '(handler-bind
           ((error (function continue)))
         (ql:quickload :mgl-pax/full))' \
      --quit -- "$@"
  else
    ros --lisp "${lisp}" run --eval '(ql:quickload :mgl-pax-test)' \
        --quit -- "$@"
  fi
  ${test_suite} ${lisp} ros --lisp ${lisp} run -- "$@"
  if ((num_failures > 0)); then
    if [ "${failure_expected}" = "t" ]; then
      echo "SHTEST: ${num_failures} expected failures,"\
           "${num_passes} passes."
    elif [ "${stop_on_failure}" = "t" ]; then
      echo "SHTEST: Aborting with ${num_failures} failures,"\
           "${num_passes} passes."
      exit 1
    fi
  fi
}

export LC_ALL=en_US.UTF-8

if [ -n "${lisp}" ]; then
  if [ "$lisp" = "sbcl" -o "$lisp" = "sbcl-bin" ]; then
      lisp2="$lisp --noinform --disable-debugger"
  elif [ "$lisp" = "ccl" -o "$lisp" = "ccl-bin" ]; then
      lisp2="$lisp --batch"
  elif [ "$lisp" = "cmucl" -o "$lisp" = "cmu-bin" ]; then
      lisp2="$lisp -batch"
  elif [ "$lisp" = "clisp" ]; then
      lisp2="$lisp -on-error exit"
  else
      lisp2="$lisp"
  fi
  run_tests lisp_tests nil ${lisp2}
else
  # Most lisps take only 10s or so to run the tests. CLISP takes 4x
  # longer. ABCL is 25x slower.
  run_tests lisp_tests nil sbcl --noinform --disable-debugger
  # run_tests nil lisp_tests allegro --batch --backtrace-on-error
  run_tests lisp_tests nil ccl-bin --batch
  run_tests lisp_tests nil cmu-bin -batch
  run_tests lisp_tests nil ecl
  run_tests lisp_tests nil clisp -on-error exit
  run_tests lisp_tests nil abcl-bin
fi

echo "SHTEST: ${num_failures} failures, ${num_passes} passes."
