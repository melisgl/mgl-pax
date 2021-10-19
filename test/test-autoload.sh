#/bin/bash -x -e

function run_tests {
sbcl --noinform --disable-debugger <<EOF
(require :asdf)
(asdf:load-system :mgl-pax)
EOF

sbcl --noinform --disable-debugger <<EOF
(require :asdf)
(asdf:load-system :mgl-pax/navigate)
EOF

sbcl --noinform --disable-debugger <<EOF
(require :asdf)
(asdf:load-system :mgl-pax/document)
EOF

sbcl --noinform --disable-debugger <<EOF
(require :asdf)
(asdf:load-system :mgl-pax/transcribe)
EOF

sbcl --noinform --disable-debugger <<EOF
(require :asdf)
(asdf:load-system :mgl-pax/full)
EOF

sbcl --noinform --disable-debugger <<EOF
(require :asdf)
(asdf:load-system :mgl-pax)
(asdf:load-system :mgl-pax/navigate)
(asdf:load-system :mgl-pax/document)
(asdf:load-system :mgl-pax/transcribe)
(asdf:load-system :mgl-pax/full)
EOF

sbcl --noinform --disable-debugger <<EOF
(require :asdf)
(asdf:load-system :mgl-pax)
(asdf:load-system :mgl-pax/navigate)
(asdf:load-system :mgl-pax/transcribe)
(asdf:load-system :mgl-pax/document)
(asdf:load-system :mgl-pax/full)
EOF

sbcl --noinform --disable-debugger <<EOF
(require :asdf)
(asdf:load-system :mgl-pax)
(asdf:load-system :mgl-pax/document)
(asdf:load-system :mgl-pax/navigate)
(asdf:load-system :mgl-pax/transcribe)
(asdf:load-system :mgl-pax/full)
EOF

sbcl --noinform --disable-debugger <<EOF
(require :asdf)
(asdf:load-system :mgl-pax)
(asdf:load-system :mgl-pax/document)
(asdf:load-system :mgl-pax/transcribe)
(asdf:load-system :mgl-pax/navigate)
(asdf:load-system :mgl-pax/full)
EOF

sbcl --noinform --disable-debugger <<EOF
(require :asdf)
(asdf:load-system :mgl-pax)
(asdf:load-system :mgl-pax/transcribe)
(asdf:load-system :mgl-pax/navigate)
(asdf:load-system :mgl-pax/document)
(asdf:load-system :mgl-pax/full)
EOF

sbcl --noinform --disable-debugger <<EOF
(require :asdf)
(asdf:load-system :mgl-pax)
(asdf:load-system :mgl-pax/transcribe)
(asdf:load-system :mgl-pax/document)
(asdf:load-system :mgl-pax/navigate)
(asdf:load-system :mgl-pax/full)
EOF

sbcl --noinform --disable-debugger <<EOF
(require :asdf)
(asdf:load-system :mgl-pax)
(mgl-pax:locate-definitions-for-emacs "mgl-pax:section" "class")
EOF

sbcl --noinform --disable-debugger <<EOF
(require :asdf)
(asdf:load-system :mgl-pax)
(mgl-pax:document #'mgl-pax:locate)
EOF

sbcl --noinform --disable-debugger <<EOF
(require :asdf)
(asdf:load-system :mgl-pax)
(mgl-pax:transcribe "*print-level*" nil)
EOF
}

# This should print only unexpected errors, warnings.
run_tests 2>&1 > /dev/null \
  | grep -v --line-buffered -x "WARNING:" \
  | grep -v --line-buffered -i redefin \
  | (grep -v --line-buffered "contains definition for system" || true)
