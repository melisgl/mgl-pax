* Testing

** Testing within Lisp

Run the Common Lisp tests with `(ASDF:TEST-SYSTEM "MGL-PAX")` or just
load `"mgl-pax/test"` and maybe `(MGL-PAX::TEST :DEBUG
'TRY:UNEXPECTED)`.

To run the Elisp tests, load `test/mgl-pax-tests.el`, `M-x ert` and
enter `"mgl-pax"` (yes, within quotes) to run only the PAX tests
(excluding Slime's). Some tests require that Slime is connected to a
Lisp and load PAX if it isn't already.

** Testing from the command line

Test the Common Lisp side with `test/test.sh`, which runs the tests on
several Lisp implementations assuming that they are installed under
Roswell (e.g. `ros --lisp sbcl run` works). So install ABCL,
AllegroCL, CCL, CMUCL, CLISP, ECL, and SBCL under Roswell.

Run the Elisp tests with `test/test-el.sh`. This currently only tests
under SBCL and needs the value of `SLIME_DIR` to be specified in the
script.

** Catching changes in behaviour

Some bugs not caught by the test suite may show up in the diffs of the
generated documentation. Check out PAX World in the top-level
directory, where the ASDF files are with `git clone
https://github.com/melisgl/mgl-pax-world.git world/`. Then, after
changing the code, regenerate the readmes and PAX World (see `#+nil`ed
out forms near the bottom of `src/document/document-util.lisp`), then
check the `git diff`s. Note that you need to `cd` into `world/` and
get the diff there too because it should be a separate git checkout.


