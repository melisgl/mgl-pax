(in-package :mgl-pax)

(in-readtable pythonic-string-syntax)

(defsection @browsing-live-documentation (:title "Browsing Live Documentation")
  """Documentation can be browsed live in Emacs or with an external
  browser. HTML documentation, complete with @CODIFICATION and
  @LINKING, is generated from docstrings of all kinds of Lisp
  definitions and PAX SECTIONs.

  If @EMACS-SETUP has been done, the Elisp function `mgl-pax-document`
  generates and displays documentation as a single HTML page. For
  example, to view the documentation of this very SECTION, one can do:

      M-x mgl-pax-document
      View Documentation of: pax::@browsing-live-documentation

  If the empty string is entered, and there is no existing w3m buffer
  or w3m is not used, then sections registered in @PAX-WORLD are
  listed. If there is a w3m buffer, then entering the empty string
  displays that buffer.

  If we enter `\function` instead, then a disambiguation page will be
  shown with the documentation of the FUNCTION class and the FUNCTION
  locative. One may then follow the links on the page to navigate to a
  page with the documentation the desired definition. If you are
  browsing live documentation right now, then the disambiguation page
  looks like this: [FUNCTION][]. In offline documentation, multiple
  links are shown instead as described in @LINKING.

  Alternatively, a DREF::@LOCATIVE may be entered as part of the
  argument to `mgl-pax-document` as in `\function class`, which gives
  [this result][function class]. Finally, the definition of DEFSECTION
  in the context of a single-page @PAX-MANUAL can be
  [viewed](pax:pax::@pax-manual#pax:defsection%20pax:macro) by
  entering `pax::@pax-manual#pax:defsection pax:macro`.

  In interactive use, `mgl-pax-document` defaults to documenting
  `slime-symbol-at-point`, possibly with a nearby locative the same
  way as in @NAVIGATING-IN-EMACS. The convenience function
  `mgl-pax-document-current-definition` documents the definition with
  point in it."""
  (@pax-urls section)
  (@apropos section)
  (@emacs-setup-for-browsing section)
  (@browsing-with-w3m section)
  (@browsing-with-other-browsers section))

(defsection @pax-urls (:title "PAX \\URLs")
  """A PAX \URL consists of a `REFERENCE` and an optional FRAGMENT
  part:

      URL = [REFERENCE] ["#" FRAGMENT]

  where `REFERENCE` names either

  - a complete DREF::@REFERENCE as a string in `NAME LOCATIVE` format
    (e.g. `"standard-object class"`),

  - or the @NAME of a reference (e.g. `"class"`), which
    possibly makes what to document ambiguous.""")

(defsection @emacs-setup-for-browsing (:title "Emacs Setup for Browsing")
  """Make sure @EMACS-SETUP has been done. In particular, set
  `mgl-pax-browser-function` to choose between browsing documentation
  with [w3m](https://emacs-w3m.github.io/info/emacs-w3m.html) in an
  Emacs buffer, or with an external browser plus a web server in the
  Lisp image.

  In @EMACS-SETUP, `(mgl-pax-hijack-slime-doc-keys)` was evaluated,
  which handles the common case of binding keys. The Elisp definition
  is reproduced here for its docstring."""
  (mgl-pax-hijack-slime-doc-keys
   (include
    (:start (nil (lambda
                   :file #.(asdf:system-relative-pathname
                            :mgl-pax "src/mgl-pax.el")
                   :snippet "(defun mgl-pax-hijack-slime-doc-keys ()"))
     :end (nil (lambda
                 :file #.(asdf:system-relative-pathname
                          :mgl-pax "src/mgl-pax.el")
                 :snippet ";; end-hijack-include")))
    :header-nl "```elisp"
    :footer-nl #.(format nil "...)~%```"))))

(define-glossary-term @w3m-key-bindings
    (:title "w3m's default key bindings"
     :url "https://emacs-w3m.github.io/info/emacs-w3m_10.html#Key-Binding"))

(defsection @browsing-with-w3m (:title "Browsing with w3m")
  """With @W3M-KEY-BINDINGS, moving the cursor between links involves
  `TAB` and `S-TAB` (or `<up>` and `<down>`). `RET` and `<right>`
  follow a link, while `B` and `<left>` go back in history.

  In addition, the following PAX-specific key bindings are available:

  - `\\M-.` visits the source location of the definition corresponding
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
  name (e.g. in `- [function] PRINT`) will raise and focus the Emacs
  window (if Emacs is not in text mode, and also subject to window
  manager focus stealing settings), then go to the corresponding
  source location. For sections, clicking on the lambda link will do
  the same (see *DOCUMENT-FANCY-HTML-NAVIGATION*).

  Finally, note that the URLs exposed by the web server are subject to
  change."""
  (*browse-html-style* variable))

(defvar *browse-html-style* :charter
  "The HTML style to use for browsing live documentation. Affects only
  non-w3m browsers. See *DOCUMENT-HTML-DEFAULT-STYLE* for the possible
  values.

  If you change this variable, you may need to do a hard refresh in
  the browser (often `C-<f5>`).")

(defun/autoloaded document-for-emacs
    (pax*-url output &optional *document-hyperspec-root*)
  (swank/backend:converting-errors-to-error-location
    (swank::with-buffer-syntax (swank::*buffer-package*)
      ;; This is called only for `w3m' and `precheck'. See below.
      (let ((*html-subformat* (if (uiop:directory-exists-p output) :w3m nil)))
        `(:url ,(document-pax*-url pax*-url output))))))

;;; Write documentation of PAX*-URL into OUTPUT. Return the (:URL
;;; <URL>) to visit or (:ERROR <STRING>). PAX*-URL may be a `pax:',
;;; `pax-eval:' or a `pax-wall' URL.
;;;
;;; - `w3m': OUTPUT denotes an existing directory. The filename within
;;;   that directory is determined by (PAX-URL-TO-FILENAME PAX*-URL)
;;;   and the returned :URL will be this `file:' (plus the
;;;   canonicalized fragment) or an external URL (e.g. to the
;;;   hyperspec). w3m then visits this URL.
;;;
;;; - `precheck': OUTPUT is NIL. No documentation is generated. This
;;;   is for redirection and error checking from Emacs before invoking
;;;   a non-w3m browser. The returned :URL is PAX*-URL canonicalized
;;;   (so that the fragment is found) or an external URL.
;;;
;;; - `web': OUTPUT denotes a file. Currently, this is used by the web
;;;    server (serving non-w3m browsers). Behaves similarly to the
;;;    `w3m' case.
(defun document-pax*-url (url output)
  (declare (type (or pathname string null) output))
  (multiple-value-bind (scheme authority path query fragment) (parse-url url)
    ;; The only QUERY is PKG handled by the web server.
    (declare (ignore query))
    (let ((place (cond ((string= scheme "pax")
                        (document-pax-url path output))
                       ((string= scheme "pax-eval")
                        (document-pax-eval-url path output))
                       ((string= scheme "pax-wall")
                        (document-pax-wall-url url path output))
                       (t
                        (assert nil)))))
      (return-url place scheme authority path fragment))))

(defun return-url (place scheme authority path fragment)
  (declare (type (or string pathname null) place))
  (let ((query
          ;; w3m doesn't like queries on file URLs, so we set
          ;; `slime-buffer-package' instead on the Elisp side.
          (unless (eq *html-subformat* :w3m)
            (format nil "pkg=~A" (urlencode (package-name *package*)))))
        (fragment (when fragment
                    (canonicalize-pax-url-fragment fragment))))
    (cond
      ;; Redirect to external URL.
      ((stringp place)
       (assert (parse-url place))
       place)
      ;; Output was written to this file.
      ((pathnamep place)
       (multiple-value-bind (f-scheme f-authority f-path f-query f-fragment)
           (parse-url (pathname-to-file-url place))
         (assert (string= f-scheme "file"))
         (assert (null f-authority))
         (assert (null f-query))
         (assert (null f-fragment))
         (make-url :scheme "file" :path f-path
                   :encoded-query query :fragment fragment)))
      ;; OUTPUT was NIL and there was no redirection to an external
      ;; URL.
      ((null place)
       ;; Return the original PAX URL with the package in QUERY and a
       ;; possibly updated FRAGMENT.
       (make-url :scheme scheme :authority authority :path path
                 :encoded-query query :fragment fragment)))))

(defun canonicalize-pax-url-fragment (fragment)
  (let ((dref (parse-dref fragment)))
    (if dref
        (dref-to-anchor dref)
        fragment)))

(defun filename-for-pax-url (output pax-url)
  (let ((filename (make-pathname :name (pax-url-to-filename pax-url)
                                 :type "html")))
    (if (uiop:directory-pathname-p output)
        (merge-pathnames filename output)
        output)))

;;; Same as *UNRESERVED-URL-CHARACTERS*, but with #\* reserved. Make
;;; that #\: reserved, too, for CCL to be happy.
(defparameter *unreserved-pax-url-filename-characters*
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
(defun pax-url-to-filename (pax-url)
  (let ((*unreserved-url-characters* *unreserved-pax-url-filename-characters*)
        (*url-escape-char* #\x))
    (urlencode pax-url)))

(defun pax-url-from-filename (filename)
  (let ((*unreserved-url-characters* *unreserved-pax-url-filename-characters*)
        (*url-escape-char* #\x))
    (urldecode filename)))


;;;; Handling of "pax-wall:" URLs. WALL is the acronym of
;;;; Word-And-Locatives-List (also, see the WALL type).

;;; pax-wall:<URLENCODED-STRINGIFIED-WALL>. The list is encoded in an
;;; URL to make the Elisp side simpler as what to document goes
;;; through `mgl-pax-w3m-goto-url'.
(defun document-pax-wall-url (pax-url path dirname)
  (let ((definitions (definitions-of-wall (read-from-string path)
                                          :definitions #'definitions*)))
    (case (length definitions)
      ((0) nil)
      ((1) (document-for-emacs/reference (first definitions) dirname))
      (t (document-for-emacs/ambiguous definitions pax-url
                                       "buffer content around point"
                                       dirname)))))


;;;; Handling of "pax-eval:" URLs

(defun document-pax-eval-url (path output)
  (let* ((form (read-from-string (urldecode path)))
         (stuff (pax-eval form)))
    (when output
      (let* ((url-path (with-standard-io-syntax*
                         (prin1-to-string form)))
             (filename (filename-for-pax-url
                        output
                        (format nil "pax-eval:~A" (urlencode url-path)))))
        (document/open/file filename stuff :title path)
        filename))))

(defun pax-eval (form)
  ;; For the sake of MGL-PAX/WEB, don't allow arbitrary evaluations.
  (unless (and (listp form)
               (member (first form) '(pax-apropos* pax-document-home-page))
               (every (lambda (arg)
                        (and (constantp arg)
                             (or (not (symbolp arg))
                                 (allowed-to-evaluate-symbol-p arg))))
                      (rest form)))
    (error "Not allowed to evaluate ~S." form))
  (eval form))

(defun allowed-to-evaluate-symbol-p (symbol)
  (or (keywordp symbol)
      (member symbol '(nil t))))

(defun make-pax-eval-url (form)
  (finalize-pax-url (format nil "pax-eval:~A"
                            (urlencode (with-standard-io-syntax*
                                         (prin1-to-string form))))))


;;;; Handling of "pax:" URLs

(defun document-pax-url (path output)
  (unless path
    (error "Nothing to document."))
  ;; FIXME: PARSE-DREF is not the inverse of DREF-TO-PAX-URL, and
  ;; neither is PARSE-DEFINITIONS* the inverse of
  ;; NAME-TO-AMBIGUOUS-PAX-URL.
  (multiple-value-bind (dref locative locative-junk) (parse-dref path)
    (declare (ignore locative))
    (cond (dref
           (document-for-emacs/reference dref output))
          (locative-junk
           (error "Unknown locative ~S." locative-junk))
          (t
           ;; Ignore LOCATIVE.
           (let ((drefs (parse-definitions* path)))
             (cond ((endp drefs)
                    (error "Could not find definitions for ~S." path))
                   ((= (length drefs) 1)
                    (document-for-emacs/reference (first drefs) output))
                   (t
                    (document-for-emacs/ambiguous
                     drefs (format nil "pax:~A" (urlencode path))
                     path output))))))))

;;; See if (DOCUMENT REFERENCE) with *DOCUMENT-OPEN-LINKING* T would
;;; try to document an external reference, and return it.
(defun open-reference-if-external (reference)
  (let ((*document-open-linking* t))
    (let ((dref (locate reference)))
      (when (external-dref-p dref)
        dref))))

;;; E.g. "pax:foo function"
(defun document-for-emacs/reference (reference output)
  (let ((reference (replace-go-target reference)))
    (if-let (external-reference (open-reference-if-external reference))
      (external-dref-url external-reference)
      (when output
        (let* ((filename (filename-for-pax-url
                          output
                          (format nil "pax:~A" (urlencode (dref-to-anchor
                                                           reference)))))
               (packagep (packagep (resolve reference nil)))
               (*package* (if packagep
                              (resolve reference)
                              *package*))
               #+nil
               (*print-arglist-key*
                 (and packagep (rcurry 'shorten-arglist reference)))
               #+nil
               (*document-docstring-key*
                 (and packagep (rcurry 'shorten-docstring reference))))
          (document/open/file filename
                              (if packagep
                                  (pax-apropos* nil t
                                                (make-symbol
                                                 (package-name *package*)))
                                  (documentable-for-reference reference))
                              :title (format nil "~A ~A"
                                             (xref-name reference)
                                             (xref-locative reference)))
          filename)))))

(defun documentable-for-reference (reference)
  (remove nil
          (append (format-up-links (sections-that-contain (list-all-sections)
                                                          reference)
                                   reference)
                  (list reference)
                  (format-also-see reference))))

#+nil
(defun shorten-arglist (string &optional except-reference)
  (let* ((reference *documenting-reference*)
         (n-chars (- 64 (length (prin1-to-string
                                 (xref-locative-type reference)))
                     (length (prin1-to-string
                              (xref-name reference))))))
    (if (and except-reference
             (reference= *documenting-reference* except-reference))
        string
        (shorten-string string :n-lines 1 :n-chars n-chars :ellipsis "..."))))

#+nil
(defun shorten-docstring (docstring &optional except-reference)
  (if (or (stringp (first *objects-being-documented*))
          (and *documenting-reference* except-reference
               (reference= *documenting-reference* except-reference)))
      docstring
      (shorten-string docstring :n-lines 1 :n-chars 68 :ellipsis "...")))

(defun format-also-see (reference)
  (let ((entries ())
        ;; This will stringify reference @NAME for e.g. PACKAGEs.
        (reference (locate reference)))
    (flet ((emit (control &rest args)
             (push (cons control args) entries)))
      (assert (not (external-dref-p reference)))
      (dolist (dref (cons (clhs-dref (dref-name reference)
                                     (dref-locative reference))
                          (definitions* reference)))
        (when (external-dref-p dref)
          (emit "the [~A][~A ~A]"
                (escape-markdown (symbol-name (dref-locative-type dref)))
                (prin1-to-markdown (dref-name dref))
                (prin1-to-markdown (dref-locative dref)))))
      (let ((generic-function-name
              (and (eq (xref-locative-type reference) 'method)
                   (xref-name reference))))
        (when generic-function-name
          (emit "the generic-function `~A`"
                (prin1-to-markdown generic-function-name))))
      (when (< 1 (length (definitions* (xref-name reference))))
        (emit "the [disambiguation page](~A)"
              (finalize-pax-url (name-to-pax-url (xref-name reference)))))
      (unless (eq (xref-locative-type reference) 'section)
        (multiple-value-bind (package other-packages)
            (find-reference-package reference)
          (when package
            (emit "the home package [~A][cl:package]"
                  (escape-markdown (package-name package))))
          (when other-packages
            (emit "other exporting packages ~{[~A][cl:package]~^, ~}"
                  (loop for package in other-packages
                        collect (escape-markdown (package-name package))))))))
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
  (let ((name (xref-name reference)))
    (when (symbolp name)
      (values (symbol-package name) (symbol-other-packages name)))))

;;; E.g. "pax:foo"
(defun document-for-emacs/ambiguous (references pax-url title output)
  (assert (< 1 (length references)))
  (when output
    (let ((filename (filename-for-pax-url output pax-url)))
      (document/open/file
       filename (cons (format nil "## Disambiguation for [~S][pax:dislocated]"
                              (escape-markdown title))
                      (dref::sort-references (replace-go-targets references)))
       :title title)
      filename)))

(defun document/open/file (filename stuff &key title)
  (with-open-file (stream (ensure-directories-exist filename)
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :external-format *utf-8-external-format*)
    (when title
      (format stream "<title>~A</title>~%" (escape-html title)))
    (document/open stuff :stream stream)))

(defvar *document/open-extra-args* ())

(defun document/open (documentable &rest args)
  (let ((*document-open-linking* t)
        (*document-fancy-html-navigation* (not (eq *html-subformat* :w3m)))
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
        for ref = (xref (section-name section) 'section)
        unless (sections-that-contain sections ref)
          collect ref))

(defun format-up-links (sections reference)
  (when sections
    (with-standard-io-syntax*
      (list
       (with-output-to-string (s)
         (format s "Up: ")
         (dolist (section (sort-by-proximity sections (xref-name reference)))
           (format s "~S " (section-name section))))))))


(defun/autoloaded redocument-for-emacs
    (file-url output &optional *document-hyperspec-root*)
  (swank/backend:converting-errors-to-error-location
    (swank::with-buffer-syntax (swank::*buffer-package*)
      ;; This is called only for `w3m' and `precheck'.
      (let ((*html-subformat* (if (uiop:directory-exists-p output) :w3m nil)))
        (multiple-value-bind (scheme authority path query fragment)
            (parse-url file-url)
          (declare (ignore authority query))
          (when (equal scheme "file")
            (assert (null fragment))
            (let ((dir (make-pathname :name nil :type nil :defaults path)))
              (when (string= (namestring dir) output)
                (let ((new-file-url (document-pax*-url
                                     (pax-url-from-filename
                                      (pathname-name path))
                                     dir)))
                  (assert (string= new-file-url file-url)))))))))
    (values)))


;;; Find the source location of the path component of PAX-URL, and
;;; return its dspec and source location. Ignore the fragment. This is
;;; what M-. in a w3m PAX doc buffer does.
(defun/autoloaded locate-pax-url-for-emacs (pax-url)
  (with-swank ()
    (swank/backend:converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (multiple-value-bind (scheme authority path) (parse-url pax-url)
          (declare (ignore authority))
          (unless (equal scheme "pax")
            (error "~S doesn't have pax: scheme." pax-url))
          (multiple-value-bind (dref locative locative-junk) (parse-dref path)
            (declare (ignore locative))
            (when locative-junk
              (error "~S in ~S is not a valid locative." locative-junk path))
            (when dref
              (let ((location (source-location dref)))
                (when (eq (first location) :location)
                  ;; List of one Swank dspec and location.
                  `((,(dref::definition-to-dspec dref) ,location)))))))))))


(defsection @apropos (:title "Apropos")
  "The Elisp functions `mgl-pax-apropos`, `mgl-pax-apropos-all`, and
  `mgl-pax-apropos-package` can display the results of
  DREF:DREF-APROPOS in the [live documentation browser]
  [@browsing-live-documentation]. These parallel the functionality of
  `slime-apropos`, `slime-apropos-all`, and `slime-apropos-package`.

  DREF:DREF-APROPOS itself is similar to CL:APROPOS-LIST, but it
  supports more flexible matching – e.g. filtering by
  DREF::@LOCATIVE-TYPEs – and returns [DRef
  references][DREF::@REFERENCE].

  The returned references are presented in two groups: those with
  non-symbol and those with symbol @NAMEs. The non-symbol group is
  sorted by locative type then by name. The symbol group is sorted by
  name then by locative type.")

;;; `mgl-pax-apropos' calls DOCUMENT-FOR-EMACS with a `pax-eval:' URL
;;; that evaluates a call to this function. NAME and PACKAGE are
;;; strings, EXTERNAL-ONLY and CASE-SENSITIVE are boolean.
(defun pax-apropos* (name &optional external-only package case-sensitive
                            (just-list t))
  (let ((name0 name)
        (package0 package))
    (flet ((find-name (string)
             (let* ((tail-pos (position #\Space string))
                    (tail (and tail-pos (subseq string (1+ tail-pos))))
                    (string (subseq string 0 tail-pos)))
               (values (cond ((string= string "") nil)
                             ((starts-with #\' string)
                              (make-symbol (subseq string 1 tail-pos)))
                             (t
                              (subseq string 0 tail-pos)))
                       tail)))
           (parse-nil-symbol-or-string (obj)
             (etypecase obj
               (string
                (let ((string obj))
                  (cond ((string= string "")
                         nil)
                        ((starts-with-subseq "'#:" string)
                         (make-symbol (subseq string 3)))
                        ((starts-with-subseq "':" string)
                         (make-symbol (subseq string 2)))
                        ((starts-with-subseq "'" string)
                         (make-symbol (subseq string 1)))
                        (t
                         string))))
               (symbol
                obj))))
      (multiple-value-bind (name locative-types) (find-name name)
        (let* ((package (parse-nil-symbol-or-string
                         (if (stringp package)
                             (dref::adjust-string-case package)
                             package)))
               ;; Whether this is for an exact package match and no
               ;; other restrictions.
               (%packagep (and package (null name) (symbolp package)))
               (locative-types (when locative-types
                                 (read-from-string
                                  (format nil "(~A)" locative-types))))
               (pax-entry-points
                 (when (and (symbolp package) (find-package package))
                   (entry-point-sections (list-sections-in-package
                                          (find-package package))))))
          (multiple-value-bind (non-symbol-definitions symbol-definitions)
              (split-apropos-definitions
               (dref-apropos name :external-only external-only
                                  :package package
                                  :case-sensitive case-sensitive
                                  :locative-types (case locative-types
                                                    ((:all)
                                                     (locative-types))
                                                    ((nil :lisp)
                                                     (lisp-locative-types))
                                                    ((:psuedo)
                                                     (pseudo-locative-types))
                                                    (t
                                                     locative-types))))
            `((progv '(*document-max-table-of-contents-level*) '(-1))
              ,@(when %packagep
                  (documentable-for-reference (xref package 'package)))
              ((progv '(*document-do-not-follow-references*)
                   ;; SECTIONs contain other sections and other
                   ;; references. Never document them in apropos to
                   ;; avoid duplications. Also exclude ASDF:SYSTEMs
                   ;; because they have headings that look weird.
                   '(,(if just-list t '(section asdf:system))))
               ,(format nil "## Apropos~%~%```~%~A~%```~%~%"
                        (let ((current-package *package*))
                          (with-standard-io-syntax*
                            (let ((*package* current-package)
                                  (*print-readably* nil)
                                  (*print-pretty* t)
                                  (*print-right-margin* 72))
                              (prin1-to-string*
                               `(dref-apropos
                                 ,(maybe-quote name)
                                 :external-only ,external-only
                                 :package ,(maybe-quote package)
                                 :case-sensitive ,case-sensitive
                                 :locative-types ,(maybe-quote
                                                   locative-types)))))))
               ,(format nil "Switch to [~S ~S](~A), [~S ~S](~A), or to ~
                            [~A view](~A)."
                        :external-only (not external-only)
                        (make-pax-eval-url
                         `(pax-apropos* ,(maybe-quote name0)
                                        ,(not external-only)
                                        ,(maybe-quote package0)
                                        ,case-sensitive ,just-list))
                        :case-sensitive (not case-sensitive)
                        (make-pax-eval-url
                         `(pax-apropos* ,(maybe-quote name0) ,external-only
                                        ,(maybe-quote package0)
                                        ,(not case-sensitive) ,just-list))
                        (if just-list "detailed" "list")
                        (make-pax-eval-url
                         `(pax-apropos* ,(maybe-quote name0) ,external-only
                                        ,(maybe-quote package0)
                                        ,case-sensitive ,(not just-list))))
               ,@(when pax-entry-points
                   (list "### PAX Entry Points"
                         (break-long-list (sections-tightly pax-entry-points))))
               ,@(when (and (not %packagep) non-symbol-definitions)
                   (cons "### Non-Symbol Definitions"
                         (list `((progv '(*document-tight*) '(t))
                                 ,@(break-long-list non-symbol-definitions)))))
               ,@(when symbol-definitions
                   (list "### Symbol Definitions"
                         `((progv '(*document-tight*) '(t))
                           ,@(break-long-list symbol-definitions))))))))))))

(defun maybe-quote (obj)
  (if (and obj (or (symbolp obj)
                   (listp obj)))
      `(quote ,obj)
      obj))

(defun split-apropos-definitions (drefs)
  ;; DREF-APROPOS returns list where non-symbol locative types are
  ;; before symbol locative types.
  (let ((pos (position-if #'symbolp drefs :key #'dref-name)))
    (if pos
        (values (subseq drefs 0 pos)
                (subseq drefs pos))
        (values drefs ()))))

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
                            (prin1-to-markdown (xref-name ref))))))

(defun pax-document-home-page ()
  (let ((*package* (find-package '#:mgl-pax) ))
    `((progv '(*package*) (list ,(find-package '#:mgl-pax)))
      ,@(list "## Documentation registered in @PAX-WORLD"
              (sections-tightly
               (mapcar #'locate (sections-registered-in-pax-world)))
              "See @BROWSING-LIVE-DOCUMENTATION for how to use this
            documentation browser."))))


(defun/autoloaded current-definition-pax-url-for-emacs
    (buffer filename possibilities)
  (with-swank ()
    (swank/backend:converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (let ((reference (find-current-definition buffer filename
                                                  possibilities)))
          (if reference
              `(:pax-url ,(dref-to-pax-url reference))
              '(:error "Cannot determine current definition.")))))))


(defun/autoloaded locatives-for-name-for-emacs (raw)
  (with-swank ()
    (swank/backend:converting-errors-to-error-location
      (swank::with-buffer-syntax ()
        (flet ((locative-to-string (locative)
                 (let ((*print-readably* nil)
                       (*print-case* :downcase))
                   (prin1-to-string locative))))
          `(:locatives
            ,(if (string= raw "")
                 (mapcar #'locative-to-string (locative-types))
                 (let ((*document-open-linking* t)
                       (locatives ()))
                   (flet ((match (name)
                            (loop for dref in (definitions* name)
                                  do (pushnew (dref-locative dref) locatives
                                              :test #'equal))))
                     (find-name #'match raw)
                     (mapcar #'locative-to-string locatives))))))))))
