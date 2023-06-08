(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @browsing-live-documentation (:title "Browsing Live Documentation")
  """Documentation can be browsed live in Emacs or with an external
  browser. HTML documentation, complete with @CODIFICATION and
  [links][@LINKING-TO-CODE], is generated from docstrings of all kinds
  of Lisp definitions and PAX SECTIONs.

  If @EMACS-SETUP has been done, the Elisp function `mgl-pax-document`
  displays documentation as a single HTML page generated by PAX via
  Slime. For example, to view the documentation of this very SECTION,
  one can do:

      M-x mgl-pax-document
      View Documentation of: pax::@browsing-live-documentation

  If the empty string is entered, and there is no existing w3m buffer
  or w3m is not used, then sections registered in @PAX-WORLD are
  listed. If there is a w3m buffer, then entering the empty string
  displays that buffer.

  If we enter `\function` instead, then a [disambiguation
  page](pax:function) (note that this and other `pax:` links only work
  in Emacs) will be shown with the documentation of the FUNCTION class
  and the FUNCTION locative. One may then follow the links on the page
  to navigate to a page with the documentation the desired definition.

  Alternatively, a @LOCATIVE may be entered as part of the argument to
  `mgl-pax-document` as in `\function class`, which gives [this
  result](pax:function%20class). Finally, the definition of DEFSECTION
  in the context of a single-page @PAX-MANUAL can be
  [viewed](pax:pax::@pax-manual#pax:defsection%20pax:macro) by
  entering `pax::@pax-manual#pax:defsection pax:macro`.

  In interactive use, `mgl-pax-document` defaults to documenting
  `slime-symbol-at-point`, possibly with a nearby locative the same
  way as in @NAVIGATING-IN-EMACS. The convenience function
  `mgl-pax-document-current-definition` documents the definition with
  point in it.
  """
  (@pax-urls section)
  (@apropos section)
  (@emacs-setup-for-browsing section)
  (@browsing-with-w3m section)
  (@browsing-with-other-browsers section))

(defsection @pax-urls (:title "PAX \\URLs")
  """A PAX \\URL consists of a \REFERENCE and an optional FRAGMENT
  part:

      URL = [REFERENCE] ["#" FRAGMENT]

  where \REFERENCE names either

  - a complete @REFERENCE (e.g. `"pax:section class"`),

  - or the @OBJECT of a reference (e.g. `"pax:section"`), which
    possibly makes what to document ambiguous.""")

(defsection @emacs-setup-for-browsing (:title "Emacs Setup for Browsing")
  """Make sure @EMACS-SETUP has been done. In particular, set
  `mgl-pax-browser-function` to choose between browsing documentation
  with [w3m](https://emacs-w3m.github.io/info/emacs-w3m.html) in an
  Emacs buffer, or with an external browser plus a web server in the
  Lisp image.

  In @EMACS-SETUP, `(mgl-pax-hijack-slime-doc-keys)` was evaluated,
  which handles the common case of binding keys. Its docstring is
  reproduced here:

      Make the following changes to `slime-doc-map' (assuming it's
      bound to `C-c C-d').

      - `C-c C-d a`: `mgl-pax-apropos` (replaces `slime-apropos`)
      - `C-c C-d z`: `mgl-pax-aproposa-all` (replaces `slime-apropos-all`)
      - `C-c C-d p`: `mgl-pax-apropos-package` (replaces
        `slime-apropos-package`)
      - `C-c C-d d`: `mgl-pax-document` (replaces `slime-describe-symbol`)
      - `C-c C-d f`: `mgl-pax-document` (replaces `slime-describe-function`)
      - `C-c C-d c`: `mgl-pax-document-current-definition`

      Also, regardless of whether `w3m` is available, add this:

      - `C-c C-d u`: `mgl-pax-edit-parent-section`

      In addition, because it can be almost as useful as `M-.`, one may
      want to give `mgl-pax-document` a more convenient binding such as
      `C-.` or `s-.` if you have a Super key. For example, to bind
      `C-.` in all Slime buffers:

          (slime-bind-keys slime-parent-map nil '((\"C-.\" mgl-pax-document)))

      To bind `C-.` globally:

          (global-set-key (kbd \"C-.\") 'mgl-pax-document)
  """)

(defsection @browsing-with-w3m (:title "Browsing with w3m")
  """With w3m's default [key bindings][w3m-key-bindings], moving the
  cursor between links involves `TAB` and `S-TAB` (or `<up>` and
  `<down>`). `RET` and `<right>` follow a link, while `B` and `<left>`
  go back in history.

    [w3m-key-bindings]: https://emacs-w3m.github.io/info/emacs-w3m_10.html#Key-Binding

  In addition, the following PAX-specific key bindings are available:

  - `M-.` visits the source location of the definition corresponding
    to the link under the point.

  - Invoking `mgl-pax-document` on a section title link will show the
    documentation of that section on its own page.

  - `n` moves to the next PAX definition on the page.

  - `p` moves to the previous PAX definition on the page.

  - `u` follows the first `Up:` link (to the first containing
    [SECTION][class]) if any.

  - `U` is like `u` but positions the cursor at the top of the page.

  - `v` visits the source location of the current definition (the one
    under the cursor or the first one above it).

  - `V` visits the source location of the first definition on the
    page.
  """)

(defsection @browsing-with-other-browsers
    (:title "Browsing with Other Browsers")
  """When the value of the Elisp variable `mgl-pax-browser-function`
  is not `w3m-browse-url`, requests are served via a web server
  started in the running Lisp, and documentation is most likely
  displayed in a separate browser window .

  By default, `mgl-pax-browser-function` is `nil`, which makes PAX use
  `browse-url-browser-function`. You may want to customize the related
  `browse-url-new-window-flag` or, for Chrome, set
  `browse-url-chrome-arguments` to `("--new-window")`.

  In the browser, clicking on the locative on the left of the
  object (e.g. in `- [function] PRINT`) will raise and focus the Emacs
  window (if Emacs is not in text mode, and also subject to window
  manager focus stealing settings), then go to the corresponding
  source location. For sections, clicking on the lambda link will do
  the same (see *DOCUMENT-FANCY-HTML-NAVIGATION*).

  Finally, note that the URLs exposed by the web server are subject to
  change.""")

;;; Document PAX*-URL in FILENAME, both STRINGs. Return a `file:' URL
;;; as (:FILE-URL <URL>) or (:ERROR <STRING>). FILENAME may denote a
;;; directory, in which case the filename will be determined by
;;; encoding PAX*-URL in it.
;;;
;;; PAX*-URL may be a `pax:', `pax-eval:' or a `pax-wall' URL.
;;;
;;; FILENAME may also be NIL, in which case no documentation is
;;; generated. This is for error checking from Emacs before launching
;;; a browser.
(defun/autoloaded document-for-emacs
    (pax*-url filename &optional *document-hyperspec-root*)
  (swank/backend:converting-errors-to-error-location
    (swank::with-buffer-syntax (swank::*buffer-package*)
      (let* ((*html-subformat* :w3m))
        `(:url ,(document-pax*-url pax*-url filename))))))

(defun document-pax*-url (url filename)
  (cond ((alexandria:starts-with-subseq "pax:" url)
         (document-pax-url-for-emacs url filename))
        ((alexandria:starts-with-subseq "pax-eval:" url)
         (document-pax-eval-url-for-emacs url filename))
        ((alexandria:starts-with-subseq "pax-wall:" url)
         (document-pax-wall-url-for-emacs url filename))
        (t
         (assert nil))))

(defun file-name-for-pax-url (file-or-dir-name pax-url)
  (let ((filename (make-pathname :name (pax-url-to-file-name pax-url)
                                 :type "html")))
    (if (uiop:directory-pathname-p file-or-dir-name)
        (merge-pathnames filename file-or-dir-name)
        (or file-or-dir-name
            ;; DOCUMENT-FOR-EMACS is being called with FILENAME NIL to
            ;; check for errors. No file should be created.
            :no-file-name))))

;;; Same as *UNRESERVED-URL-CHARACTERS*, but with #\* reserved. Make
;;; that #\: reserved, too, for CCL to be happy.
(defparameter *unreserved-pax-url-file-name-characters*
  (let ((array (make-array 256 :element-type 'bit :initial-element 0)))
    (_mark-range array #\a #\z)
    (_mark-range array #\A #\Z)
    (_mark-range array #\0 #\9)
    (_mark-one array #\-)
    (_mark-one array #\_)
    (_mark-one array #\.)
    (_mark-one array #\@)
    (_mark-one array #\+)
    array))

;;; A PAX URL is like pax:PATH[#FRAGMENT]. When the documentation is
;;; written to a file, special characters in PATH must be somehow
;;; escaped so that the result is a valid file name. Also, since w3m's
;;; URL history is buggy with regards to URL encoding and decoding,
;;; the encoded PATH must have no #\% in it.
(defun pax-url-to-file-name (pax-url)
  (let ((*unreserved-url-characters* *unreserved-pax-url-file-name-characters*)
        (*url-escape-char* #\x))
    (urlencode pax-url)))

(defun pax-url-from-file-name (filename)
  (let ((*unreserved-url-characters* *unreserved-pax-url-file-name-characters*)
        (*url-escape-char* #\x))
    (urldecode filename)))


;;;; Handling of "pax-wall:" URLs. WALL is the acronym of
;;;; Word-And-Locatives-List (also, see the WALL type).

;;; pax-wall:<URLENCODED-STRINGIFIED-WALL>. The list is encoded in an
;;; URL to make the Elisp side simpler as what to document goes
;;; through `mgl-pax-w3m-goto-url'.
(defun document-pax-wall-url-for-emacs (pax-wall-url filename)
  (multiple-value-bind (scheme authority path) (parse-url pax-wall-url)
    (declare (ignore authority))
    (unless (equal scheme "pax-wall")
      (error "~S doesn't have pax-wall: scheme." pax-wall-url))
    (let ((definitions (definitions-of-wall (read-from-string (urldecode path))
                                            :definitions-of 'documentables-of)))
      (case (length definitions)
        ((0) nil)
        ((1)
         (urlify-if-pathname
          (document-for-emacs/reference (first definitions) filename)))
        (t
         (urlify-if-pathname
          (document-for-emacs/ambiguous definitions pax-wall-url
                                        "buffer content around point"
                                        filename)))))))

(defun documentables-of (object)
  (mapcar #'link-reference
          (remove-duplicates (mapcar #'unaliased-link
                                     (let ((*document-open-linking* t))
                                       (links-of object))))))

(defun urlify-if-pathname (pathname-or-url &optional fragment)
  (if (pathnamep pathname-or-url)
      (if fragment
          (format nil "~A#~A" (pathname-to-file-url pathname-or-url)
                  (canonicalize-pax-url-fragment fragment))
          (pathname-to-file-url pathname-or-url))
      pathname-or-url))

(defun canonicalize-pax-url-fragment (fragment)
  (multiple-value-bind (object locative foundp)
      (read-reference-from-string fragment)
    (urlencode
     (if foundp
         (reference-to-anchor (canonical-reference
                               (make-reference object locative)))
         fragment))))


;;;; Handling of "pax-eval:" URLs

(defun document-pax-eval-url-for-emacs (pax-eval-url filename)
  (multiple-value-bind (scheme authority path) (parse-url pax-eval-url)
    (declare (ignore authority))
    (unless (equal scheme "pax-eval")
      (error "~S doesn't have pax-eval: scheme." pax-eval-url))
    (let* ((form (read-from-string (urldecode path)))
           (stuff (pax-eval form))
           (filename (file-name-for-pax-url
                      filename
                      (format nil "pax-eval:~A"
                              (urlencode (with-standard-io-syntax*
                                           (prin1-to-string form)))))))
      (document/open/file filename stuff :title path)
      (if (eq filename :no-file-name)
          filename
          (pathname-to-file-url filename)))))

(defun pax-eval (form)
  ;; For the sake of MGL-PAX/WEB, don't allow arbitrary evaluations.
  (unless (and (listp form)
               (eq (first form) 'pax-apropos*))
    (error "Not allowed to evaluate ~S." form))
  (eval form))

(defun make-pax-eval-url (form)
  (finalize-pax-url (format nil "pax-eval:~A"
                            (urlencode (with-standard-io-syntax*
                                         (prin1-to-string form))))))


;;;; Handling of "pax:" URLs

(defun document-pax-url-for-emacs (pax-url filename)
  (multiple-value-bind (scheme authority path query fragment)
      (parse-url pax-url)
    (declare (ignore authority query))
    (unless (equal scheme "pax")
      (error "~S doesn't have pax: scheme." pax-url))
    (unless path
      (error "Nothing to document."))
    (urlify-if-pathname (document-pax-url-path path filename) fragment)))

(defun document-pax-url-path (path filename)
  (multiple-value-bind (object locative foundp locative-junk)
      (read-reference-from-string path)
    (cond (foundp
           (document-for-emacs/reference (pax:make-reference object locative)
                                         filename))
          (locative-junk
           (error "Unknown locative ~S." locative-junk))
          (t
           (let ((references (documentables-of (read-from-string path))))
             (cond ((endp references)
                    (error "Could not find definitions for ~S." path))
                   ((= (length references) 1)
                    (document-for-emacs/reference (first references) filename))
                   (t
                    (document-for-emacs/ambiguous
                     references (format nil "pax:~A" (urlencode path))
                     path filename))))))))

;;; See if (DOCUMENT REFERENCE) with *DOCUMENT-OPEN-LINKING* T would
;;; try to document an external reference, and return it.
(defun open-reference-if-external (reference)
  (let ((*document-open-linking* t))
    (let ((reference (canonical-reference reference)))
      (when (external-locative-p (reference-locative reference))
        (resolve reference)))))

;;; E.g. "pax:foo function"
(defun document-for-emacs/reference (reference filename)
  (let ((reference (replace-go-target reference)))
    (alexandria:if-let (external-reference (open-reference-if-external
                                            reference))
      (external-reference-url external-reference)
      (let* ((stuff ())
             (filename (file-name-for-pax-url
                        filename
                        (format nil "pax:~A" (urlencode (reference-to-anchor
                                                         reference)))))
             (all-sections (list-all-sections))
             (sections (sections-that-contain all-sections reference))
             (packagep (packagep (resolve reference :errorp nil)))
             (*package* (if packagep
                            (resolve reference :errorp nil)
                            *package*))
             #+nil
             (*print-arglist-key*
               (and packagep (alexandria:rcurry 'shorten-arglist reference)))
             #+nil
             (*document-docstring-key*
               (and packagep (alexandria:rcurry 'shorten-docstring reference))))
        (alexandria:appendf stuff (format-up-links sections reference))
        (alexandria:appendf stuff (list reference))
        (alexandria:appendf stuff (format-also-see reference))
        (document/open/file filename (remove nil stuff)
                            :title (format nil "~A ~A"
                                           (reference-object reference)
                                           (reference-locative reference)))
        filename))))

#+nil
(defun shorten-arglist (string &optional except-reference)
  (let* ((reference *reference-being-documented*)
         (n-chars (- 64 (length (prin1-to-string
                                 (reference-locative-type reference)))
                     (length (prin1-to-string
                              (reference-object reference))))))
    (if (and except-reference
             (reference= *reference-being-documented* except-reference))
        string
        (shorten-string string :n-lines 1 :n-chars n-chars :ellipsis "..."))))

#+nil
(defun shorten-docstring (docstring &optional except-reference)
  (if (or (stringp (first *objects-being-documented*))
          (and *reference-being-documented* except-reference
               (reference= *reference-being-documented* except-reference)))
      docstring
      (shorten-string docstring :n-lines 1 :n-chars 68 :ellipsis "...")))

(defun format-also-see (reference)
  (let ((entries ())
        ;; This will stringify reference @OBJECTs for e.g. PACKAGEs.
        (reference (canonical-reference reference)))
    (flet ((emit (control &rest args)
             (push (cons control args) entries)))
      (assert (not (external-reference-p reference)))
      (dolist (link (links-of reference))
        (let ((reference (link-reference (unaliased-link link))))
          (when (external-reference-p reference)
            (emit "the [~A][~A ~A]"
                  (escape-markdown (symbol-name
                                    (reference-locative-type reference)))
                  (prin1-to-markdown (reference-object reference))
                  (prin1-to-markdown (reference-locative reference))))))
      (let ((generic-function-name
              (and (eq (reference-locative-type reference) 'method)
                   (reference-object reference))))
        (when generic-function-name
          (emit "the generic-function `~A`"
                (prin1-to-markdown generic-function-name))))
      (when (< 1 (length (definitions-of (reference-object reference))))
        (emit "the [disambiguation page](~A)"
              (finalize-pax-url (urlencode (reference-to-ambiguous-pax-url
                                            reference)))))
      (unless (eq (reference-locative-type reference) 'section)
        (let ((package (find-reference-package reference)))
          (when package
            (emit "the package `~A`"
                  (escape-markdown (package-name package))))))
      (let ((resolved (resolve reference :errorp nil)))
        (when (packagep resolved)
          (let ((name (make-symbol (package-name resolved))))
            (emit "see the package [apropos](~A), ~
                   maybe [with internal symbols](~A) included"
                  (make-pax-eval-url `(pax-apropos* nil t ',name t))
                  (make-pax-eval-url `(pax-apropos* nil nil ',name t)))))))
    (when entries
      (list
       (let ((n-entries (length entries)))
         (with-output-to-string (out)
           (format out "Also, see")
           (loop for entry in (reverse entries)
                 for i upfrom 0
                 do (format out (if (and (< 2 n-entries) (plusp i))
                                    ", "
                                    " "))
                    (when (and (< 1 n-entries)
                               (= i (1- n-entries)))
                      (format out "and "))
                    (apply #'format out entry))
           (format out ".~%")))))))

(defun find-reference-package (reference)
  (let ((object (reference-object reference)))
    (when (symbolp object)
      (symbol-package object))))

;;; E.g. "pax:foo"
(defun document-for-emacs/ambiguous (references pax-url title filename)
  (assert (< 1 (length references)))
  (let ((filename (file-name-for-pax-url filename pax-url)))
    (document/open/file
     filename (cons (format nil "## Disambiguation for [`~A`][pax:dislocated]"
                            (escape-markdown title))
                    (sort-references (replace-go-targets references)))
     :title title)
    filename))

(defun document/open/file (filename stuff &key title)
  (unless (or (eq filename nil) (eq filename :no-file-name))
    (with-open-file (stream (ensure-directories-exist filename)
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede
                            :external-format *utf-8-external-format*)
      (when title
        (format stream "<title>~A</title>~%" (escape-html title)))
      (document/open stuff :stream stream))))

(defvar *document/open-extra-args* ())

(defun document/open (documentable &rest args)
  (let ((*document-open-linking* t)
        (*document-fancy-html-navigation* (not (eq *html-subformat* :w3m)))
        (*document-normalize-packages* t)
        (*document-url-versions* '(2))
        (previous-error-string "")
        (n-repeats 0))
    (handler-bind
        ;; CONTINUE continuable errors, but give up if the same error
        ;; seems to happen again to avoid getting stuck and maybe
        ;; running out of stack.
        ((error
           (lambda (error)
             (let ((string (with-standard-io-syntax*
                             (princ-to-string error))))
               (if (string= string previous-error-string)
                   (incf n-repeats)
                   (setq n-repeats 0))
               (setq previous-error-string string)
               (warn "~@<Error in ~S: ~A~:@>" 'document error))
             (if (< 100 n-repeats)
                 (warn "~@<Sameish error repeated too many times. ~
                                 Not CONTINUEing.~:@>")
                 (continue error)))))
      (apply #'document documentable (append args (list :format :html)
                                             *document/open-extra-args*)))))


;;;; Listing SECTIONs

(defun list-sections-in-package (package)
  (let ((sections ()))
    (do-symbols (symbol package sections)
      (when (boundp symbol)
        (let ((value (symbol-value symbol)))
          (when (and (typep value 'section)
                     ;; Filter out normal variables with SECTION values.
                     (eq (section-name value) symbol))
            (pushnew value sections)))))))

(defun entry-point-sections (sections)
  (loop for section in sections
        for ref = (make-reference (section-name section) 'section)
        unless (sections-that-contain sections ref)
          collect ref))

(defun format-up-links (sections reference)
  (when sections
    (with-standard-io-syntax*
      (list
       (with-output-to-string (s)
         (format s "Up: ")
         (dolist (section (sort-by-proximity sections reference))
           (format s "<a href='~A#~A'>~A</a> "
                   (finalize-pax-url
                    (urlencode (reference-to-pax-url
                                (canonical-reference section))))
                   (urlencode (reference-to-anchor reference))
                   (section-title-or-name section))))))))


(defun/autoloaded redocument-for-emacs
    (file-url dirname &optional *document-hyperspec-root*)
  (swank/backend:converting-errors-to-error-location
    (swank::with-buffer-syntax (swank::*buffer-package*)
      (let* ((*html-subformat* :w3m))
        (multiple-value-bind (scheme authority path query fragment)
            (parse-url file-url)
          (declare (ignore authority query))
          (when (equal scheme "file")
            (assert (null fragment))
            (let ((dir (make-pathname :name nil :type nil :defaults path)))
              (when (string= (namestring dir) dirname)
                (let ((new-file-url (document-pax*-url
                                     (pax-url-from-file-name
                                      (pathname-name path))
                                     dir)))
                  (assert (string= new-file-url file-url)))))))))
    (values)))


;;; Locate the path component. Ignore the fragment. This is what M-.
;;; in a w3m PAX doc buffer does.
(defun/autoloaded locate-pax-url-for-emacs (pax-url)
  (with-swank ()
    (swank/backend:converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (multiple-value-bind (scheme authority path) (parse-url pax-url)
          (declare (ignore authority))
          (unless (equal scheme "pax")
            (error "~S doesn't have pax: scheme." pax-url))
          (multiple-value-bind (object locative foundp)
              (read-reference-from-string path)
            (unless foundp
              (error "Could not parse ~S as a reference." path))
            (let* ((ref (make-reference object locative))
                   (location (find-source ref)))
              (when (eq (first location) :location)
                ;; List of one Swank dspec and location.
                `((,(reference-to-dspec ref) ,location))))))))))


(defsection @apropos (:title "PAX Apropos")
  "PAX-APROPOS is similar to CL:APROPOS-LIST, but it supports more
  flexible matching – especially filtering by @LOCATIVE-TYPES – and
  returns REFERENCEs.

  On the Emacs side, `mgl-pax-apropos`, `mgl-pax-apropos-all`, and
  `mgl-pax-apropos-package` can be used to view the results in the
  [documentation browser][@browsing-live-documentation]. These
  parallel the functionality of `slime-apropos`, `slime-apropos-all`,
  and `slime-apropos-package`."
  (pax-apropos function))

(defun pax-apropos (name &key package external-only case-sensitive
                           locative-types)
  "Return a list of REFERENCEs corresponding to definitions of symbols
  matching various arguments. As the second value, return another list
  of REFERENCEs that correspond to definitions named by string such as
  PACKAGEs and ASDF:SYSTEMs.

  First, from the set of all interned symbols, the set of matching
  @OBJECTs are determined:

  - NAME is NIL (matches everything), a SYMBOL (matches the same
    SYMBOL-NAME), or a STRING (matches a symbol if it's a substring of
    SYMBOL-NAME subject to CASE-SENSITIVE).

  - PACKAGE is NIL (matches everything), a SYMBOL (matches the same
    PACKAGE-NAME or a nickname), or a [PACKAGE][class] (matches a
    symbol if it's a substring of the name of SYMBOL-PACKAGE).

  - EXTERNAL-ONLY is NIL (matches everything), or T (matches only
    symbols which are external in their home package).

  Then, for all matching @OBJECTS, their known definitions are
  collected as a single list of REFERENCEs. If LOCATIVE-TYPES is not
  NIL, then REFERENCEs whose LOCATIVE-TYPE is not in LOCATIVE-TYPES
  are removed from the list. Finally, the list is sorted preferring
  symbols accessible in the current package, alphabetically earlier
  package names, and alphabetically earlier symbol names, in that
  order.

  For the second list, names of registered ASDF:SYSTEMs and PACKAGEs
  are matched against NAME, the PACKAGE and EXTERNAL-ONLY arguments
  are ignored. This list is also filtered by LOCATIVE-TYPES and sorted
  alphabetically by LOCATIVE-TYPE name. This is list always empty if
  PACKAGE."
  (let ((test (if case-sensitive #'char= #'char-equal))
        (matching-symbols (make-hash-table))
        (asdf-definitions ())
        (package-definitions ()))
    (labels ((matching-package-p (package-1)
               (or (null package)
                   (and (symbolp package)
                        (find (symbol-name package)
                              (cons (package-name package-1)
                                    (package-nicknames package-1))
                              :test (if case-sensitive
                                        #'string=
                                        #'string-equal)))
                   (and (stringp package)
                        (find-if (lambda (package-name-1)
                                   (search package package-name-1 :test test))
                                 (cons (package-name package-1)
                                       (package-nicknames package-1))))
                   (and (packagep package)
                        (eq package-1 package))))
             (matching-name-p (name-1)
               (or (null name)
                   (and (symbolp name)
                        (if case-sensitive
                            (string= (symbol-name name) name-1)
                            (string-equal (symbol-name name) name-1)))
                   (and (stringp name)
                        (search name name-1 :test test))))
             (matching-symbol-reference-p (reference)
               (let ((locative-type (reference-locative-type reference)))
                 (and (or (null locative-types)
                          (member locative-type locative-types))
                      (not (member locative-type '(package asdf:system))))))
             (consider (symbol)
               (when (matching-name-p (symbol-name symbol))
                 (setf (gethash symbol matching-symbols) t))))
      ;; Collect matching symbols, but only if we are going to use
      ;; them.
      (when (or (null locative-types)
                (remove 'asdf:system (remove 'package locative-types)))
        (dolist (package-1 (remove (find-package :keyword)
                                   (list-all-packages)))
          (when (matching-package-p package-1)
            (if external-only
                (with-package-iterator (next package-1 :external)
                  (loop (multiple-value-bind (morep symbol) (next)
                          (if morep
                              (consider symbol)
                              (return)))))
                (with-package-iterator (next package-1 :external :internal)
                  (loop (multiple-value-bind (morep symbol) (next)
                          (if morep
                              (consider symbol)
                              (return)))))))))
      ;; FIXME: Add a generic function to enumerate possible
      ;; non-symbol @OBJECTs for a given LOCATIVE-TYPE?
      (unless package
        ;; ASDF:SYSTEM locative
        (when (or (null locative-types)
                  (member 'asdf:system locative-types))
          (dolist (system-name (asdf:registered-systems))
            (when (matching-name-p system-name)
              (push (make-reference system-name 'asdf:system)
                    asdf-definitions))))
        ;; PACKAGE locative
        (when (or (null locative-types)
                  (member 'package locative-types))
          (dolist (package-1 (list-all-packages))
            (when (matching-name-p (package-name package-1))
              (push (canonical-reference package-1) package-definitions)))))
      (values (remove-if-not #'matching-symbol-reference-p
                             (mapcan #'definitions-of
                                     (sort (alexandria:hash-table-keys
                                            matching-symbols)
                                           #'swank::present-symbol-before-p)))
              (append (sort asdf-definitions #'string<
                            :key #'reference-object)
                      (sort package-definitions #'string<
                            :key #'reference-object))))))

;;; `mgl-pax-apropos' calls DOCUMENT-FOR-EMACS with a `pax-eval:' URL
;;; that evaluates a call to this function. NAME and PACKAGE are
;;; strings, EXTERNAL-ONLY and CASE-SENSITIVE are boolean.
(defun pax-apropos* (name &optional external-only package case-sensitive)
  (flet ((parse-name (string)
           (let* ((tail-pos (position #\Space string))
                  (tail (and tail-pos (subseq string (1+ tail-pos))))
                  (string (subseq string 0 tail-pos)))
             (values (cond ((string= string "") nil)
                           ((alexandria:starts-with #\' string)
                            (make-symbol (subseq string 1 tail-pos)))
                           (t
                            (subseq string 0 tail-pos)))
                     tail)))
         (parse-nil-symbol-or-string (string)
           (cond ((string= string "")
                  nil)
                 ((alexandria:starts-with #\' string)
                  (make-symbol (subseq string 1)))
                 (t
                  string))))
    (multiple-value-bind (name locative-types) (parse-name name)
      (let ((package (parse-nil-symbol-or-string package))
            (locative-types (when locative-types
                              (read-from-string
                               (format nil "(~A)" locative-types)))))
        (multiple-value-bind (symbol-definitions non-symbol-definitions)
            (pax-apropos name :external-only external-only
                              :package package
                              :case-sensitive case-sensitive
                              :locative-types locative-types)
          (let ((asdf-definitions
                  (remove 'asdf:system non-symbol-definitions
                          :key #'reference-locative-type :test-not 'eq))
                (package-definitions
                  (remove 'package non-symbol-definitions
                          :key #'reference-locative-type :test-not 'eq))
                (non-symbol-definitions
                  (remove 'package (remove 'asdf:system non-symbol-definitions
                                           :key #'reference-locative-type)
                          :key #'reference-locative-type))
                (pax-entry-points
                  (when (and (symbolp package)
                             case-sensitive
                             (find-package package))
                    (entry-point-sections (list-sections-in-package
                                           (find-package package))))))
            `((progv '(*document-do-not-resolve-references*) '(t))
              (,(format nil "## Apropos~%~%```~%~A~%```~%"
                        (let ((current-package *package*))
                          (with-standard-io-syntax*
                            (let ((*package* current-package)
                                  (*print-readably* nil)
                                  (*print-pretty* t)
                                  (*print-right-margin* 72))
                              (prin1-to-markdown
                               `(pax-apropos
                                 ,(if (and name (symbolp name))
                                      `(quote ,name)
                                      name)
                                 :external-only ,external-only
                                 :package ,(if (and package (symbolp package))
                                               `(quote ,package)
                                               package)
                                 :case-sensitive ,case-sensitive
                                 :locative-types ,(if locative-types
                                                      `(quote ,locative-types)
                                                      nil))
                               :escape-newline nil)))))
               ,@(when asdf-definitions
                   (list "### \\ASDF systems"
                         `((progv '(*document-tight*) '(t))
                           ,@(break-long-list asdf-definitions))))
               ,@(when package-definitions
                   (list "### Packages"
                         `((progv '(*document-tight*) '(t))
                           ,@(break-long-list package-definitions))))
               ,@(when non-symbol-definitions
                   (list "### Non-symbol definitions"
                         `((progv '(*document-tight*) '(t))
                           ,@(break-long-list non-symbol-definitions))))
               ,@(when pax-entry-points
                   (list "### PAX Entry Points"
                         (break-long-list (sections-tightly pax-entry-points))))
               ,@(when symbol-definitions
                   (list "### Symbol definitions"
                         `((progv '(*document-tight*) '(t))
                           ,@(break-long-list symbol-definitions))))))))))))

;;; Workaround for PARSE-MARKDOWN-FAST being slow on large lists.
(defun break-long-list (list &key (n 10))
  (let ((len (length list)))
    (loop for i upfrom 0 by n below len
          append (let ((group (subseq list i (min (+ i n) len))))
                   (if (plusp i)
                       (cons `((progv '(*document-tight*) '(nil))
                               ,(format nil "~%~%<span/>~%"))
                             group)
                       group)))))

(defun sections-tightly (section-refs)
  `((progv '(*document-tight*) '(t))
    ,@(loop for ref in section-refs
            collect (format nil "- [~A][pax:section]"
                            (prin1-to-markdown
                             (reference-object ref))))))

(defun pax-document-home-page ()
  `((progv '(*package*) (list ,(find-package '#:mgl-pax)))
    ,@(list "## Documentation registered in @PAX-WORLD"
            (sections-tightly
             (mapcar #'canonical-reference (sections-registered-in-pax-world)))
            "See @BROWSING-LIVE-DOCUMENTATION for how to use this
            documentation browser.")))


(defun/autoloaded current-definition-pax-url-for-emacs
    (buffer filename possibilities)
  (with-swank ()
    (swank/backend:converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (let ((reference (find-current-definition buffer filename
                                                  possibilities)))
          (if reference
              `(:pax-url ,(reference-to-pax-url reference))
              '(:error "Cannot determine current definition.")))))))
