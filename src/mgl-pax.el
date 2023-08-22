;; -*- lexical-binding: t -*-

;;;; MGL-PAX Emacs integration
;;;; =========================
;;;;
;;;; SETUP (see MGL-PAX::@EMACS-SETUP)
;;;; ---------------------------------
;;;;
;;;; - `mgl-pax-autoload'
;;;;
;;;; - `mgl-pax-reload'
;;;;
;;;; - `mgl-pax-hijack-slime-doc-keys'
;;;;
;;;; - `mgl-pax-browser-function'
;;;;
;;;; - `mgl-pax-web-server-port'
;;;;
;;;; NAVIGATE (see MGL-PAX::@NAVIGATING-IN-EMACS)
;;;; --------------------------------------------
;;;;
;;;; - `M-.' (`slime-edit-definition') supports new kinds of
;;;;   definitions (e.g. of ASDF/SYSTEMs) and disambiguates based on
;;;;   nearby locatives. Just by loading this file, `M-.' shall be
;;;;   able to recognize disambiguate based on locatives near point as
;;;;   in "function FOO".
;;;;
;;;; - Also, see `mgl-pax-edit-parent-section'.
;;;;
;;;; DOCUMENT (see MGL-PAX::@BROWSING-LIVE-DOCUMENTATION)
;;;; ----------------------------------------------------
;;;;
;;;; - Browse documentation of definitions in the running Lisp live
;;;;   without explicitly generating documentation with
;;;;   `mgl-pax-document'. Bind it to `C-.' to parallel `M-.'.
;;;;
;;;; - Also, see `mgl-pax-current-definition-toggle-view'.
;;;;
;;;; - `mgl-pax-apropos', `mgl-pax-apropos-all' and
;;;;   `mgl-pax-apropos-package' are replacements for `slime-apropos'
;;;;   `slime-apropos-all' and `slime-apropos-package', respectively.
;;;;   They are all built on top of `mgl-pax-document'.
;;;;
;;;; TRANSCRIBE (see MGL-PAX::@TRANSCRIBING-WITH-EMACS (press `C-.' on this))
;;;; ------------------------------------------------------------------------
;;;;
;;;; - For `mgl-pax-transcribe-last-expression' and
;;;;   `mgl-pax-retranscribe-region'.

(eval-and-compile
  (require 'cl-lib nil t)
  ;; For emacs 23, look for bundled version
  (require 'cl-lib "lib/cl-lib")
  (require 'slime))


;;;; Autoloading of MGL-PAX on the Common Lisp side

(defcustom mgl-pax-autoload t
  "If true, then the MGL-PAX ASDF system will be loaded as necessary
via Slime by `slime-edit-definition', `mgl-pax-document' and
other mgl-pax commands. Furthermore, when
`mgl-pax-browser-function' is not 'w3m-browse-url',
`mgl-pax-document' will start a web server on the Common Lisp
side."
  :type 'boolean
  :group 'mgl-pax)

(defvar mgl-pax-version)
(setq mgl-pax-version  '(0 3 0))

(defun mgl-pax-maybe-autoload (no-web cont)
  (if (or no-web (mgl-pax-use-w3m))
      (mgl-pax-maybe-autoload-1 cont)
    (mgl-pax-ensure-web-server cont)))

(defun mgl-pax-maybe-autoload-1 (cont)
  (let ((check-version-form
         `(cl:and (cl:find-package :mgl-pax)
                  (cl:funcall (cl:find-symbol
                               (cl:string '#:check-pax-elisp-version)
                               (cl:find-package :mgl-pax))
                              ',mgl-pax-version)
                  t)))
    (if mgl-pax-autoload
        (slime-eval-async
            `(cl:progn
              (cl:unless
               (cl:and (cl:find-package :mgl-pax)
                       ;; Not there if only mgl-pax-bootstrap is loaded.
                       (cl:find-symbol
                        (cl:string '#:check-pax-elisp-version)
                        (cl:find-package :mgl-pax)))
               (cl:format t "~&;; Autoloading MGL-PAX for Emacs ~
                            (mgl-pax-autoload is t).~%")
               (asdf:load-system "mgl-pax")
               (cl:format t ";; Done autoloading MGL-PAX for Emacs~%"))
              ,check-version-form)
          cont)
      (slime-eval-async check-version-form cont))))

(cl-defmacro mgl-pax-ensure-pax-loaded ((&key no-web) &body body)
  (declare (indent 1))
  `(mgl-pax-maybe-autoload ,no-web (lambda (loadedp)
                                     (if (not loadedp)
                                         (mgl-pax-not-loaded)
                                       ,@body))))

(defun mgl-pax-not-loaded ()
  (message "MGL-PAX is not loaded. See the variable mgl-pax-autoload."))

(defvar mgl-pax-file-name)
(setq mgl-pax-file-name load-file-name)

(defun mgl-pax-reload ()
  "Reload mgl-pax.el. This may be necessary after upgrading MGL-PAX.
See MGL-PAX::@EMACS-SETUP."
  (interactive)
  (let ((sourcefile (concat (file-name-sans-extension mgl-pax-file-name)
                            ".el")))
    (load-file sourcefile)))


(defun mgl-pax-hijack-slime-doc-keys ()
  "Make the following changes to `slime-doc-map' (assuming it's
bound to `C-c C-d').

- `C-c C-d a': `mgl-pax-apropos' (replaces `slime-apropos')
- `C-c C-d z': `mgl-pax-aproposa-all' (replaces `slime-apropos-all')
- `C-c C-d p': `mgl-pax-apropos-package' (replaces `slime-apropos-package')
- `C-c C-d d': `mgl-pax-document' (replaces `slime-describe-symbol')
- `C-c C-d f': `mgl-pax-document' (replaces `slime-describe-function')
- `C-c C-d c': `mgl-pax-current-definition-toggle-view'

Also, regardless of whether `w3m' is available, add this:

- `C-c C-d u': `mgl-pax-edit-parent-section'

In addition, because it can be almost as useful as `M-.', one may
want to give `mgl-pax-document' a more convenient binding such as
`C-.' or `s-.' if you have a Super key. For example, to bind
`C-.' in all Slime buffers:

    (slime-bind-keys slime-parent-map nil '((\"C-.\" mgl-pax-document)))

To bind `C-.' globally:

    (global-set-key (kbd \"C-.\") 'mgl-pax-document)"
  ;; end-hijack-include
  (interactive)
  (slime-bind-keys slime-doc-map t
                   '((?a mgl-pax-apropos)
                     (?z mgl-pax-apropos-all)
                     (?p mgl-pax-apropos-package)
                     (?d mgl-pax-document)
                     (?f mgl-pax-document)
                     (?c mgl-pax-current-definition-toggle-view)))
  (slime-bind-keys slime-doc-map t
                   '((?u mgl-pax-edit-parent-section))))


;;;; Browser configuration

(defcustom mgl-pax-browser-function nil
  "The name of the function to use to browse URLs.
When nil, the value of `browse-url-browser-function' is used. If
the effective value is `w3m-browse-url', then browsing will take
place in Emacs buffers using `w3m', and no webserver will be run
on the Common Lisp side."
  :type 'symbol
  :group 'mgl-pax)

(defcustom mgl-pax-web-server-port nil
  "If the web server is started, it will be on this port.
See `mgl-pax-autoload'. If nil, then a free port will be used."
  :type 'natnum
  :group 'mgl-pax)

(defun mgl-pax-use-w3m ()
  (eq (or mgl-pax-browser-function browse-url-browser-function)
      'w3m-browse-url))

(defvar mgl-pax-web-server-base-url)

(defun mgl-pax-ensure-web-server (cont)
  (mgl-pax-maybe-autoload-1
   (lambda (loadedp)
     (if (not loadedp)
         (funcall cont nil)
       (slime-eval-async `(mgl-pax::ensure-web-server
                           :hyperspec-root ',common-lisp-hyperspec-root
                           :port ,mgl-pax-web-server-port)
         (lambda (values)
           (if (eq (cl-first values) :error)
               (message "%s" (cl-second values))
             (cl-assert (eq (cl-first values) :base-url))
             (setq mgl-pax-web-server-base-url (cl-second values))
             (funcall cont t))))))))



;;;; Find possible objects and locatives at point (see MGL-PAX::WALL).

;;; Return a list of of things like (object (locative1 locative2 ...))
;;; representing the possible references (object locative1), (object
;;; locative2), and so on. MGL-PAX::LOCATE-DEFINITIONS-FOR-EMACS and
;;; MGL-PAX::DOCUMENT-FOR-EMACS take such lists.
;;;
;;; `slime-symbol-at-point' works fine in code, but in printed
;;; representations and docstrings heuristics are needed (just think
;;; "SYM." and "#<SYM"), which we leave for the the Common Lisp side
;;; to resolve. However, we handle here the complications caused by
;;; Markdown, whose code (`nil`) and reference link syntax
;;; ([title][id]) is used by PAX, maybe both at the same time as in
;;; [`FOO`][function] or [FOO][`function`]. ?` is a delimiter, but ?\[
;;; is not, which means that `slime-symbol-at-point' on FOO will
;;; result in NAME being "FOO" or "[FOO][". "[FOO][" is a valid symbol
;;; name, so we definitely want to look up definitions for it. In
;;; addition, we also look up definitions for the symbol whose name
;;; has the parts beyond [] cut off.
(defun mgl-pax-wall-at-point ()
  ;; `mgl-pax-call-in-lisp-mode' makes this function work even in
  ;; non-lisp buffers.
  (mgl-pax-call-in-lisp-mode
   (lambda ()
     ;; So that locatives spanning multiple comment lines are parsed
     ;; without the semicolons.
     (mgl-pax-call-uncommented
      (lambda ()
        (let ((name (slime-symbol-at-point))
              (bounds (slime-bounds-of-symbol-at-point)))
          (when bounds
            (let ((locatives (mgl-pax-find-locatives bounds))
                  (wall (mgl-pax-parse-reflink bounds)))
              (append (and name `((,name ,locatives))) wall)))))))))

;;; If not in a lisp-mode buffer, then copy the five lines around
;;; point into a temporary lisp buffer, put point on the same
;;; character in the new buffer and call FN.
(defun mgl-pax-call-in-lisp-mode (fn)
  (if (eq major-mode 'lisp-mode)
      (funcall fn)
    (let ((text-before (buffer-substring-no-properties
                        (line-beginning-position -1)
                        (point)))
          (text-after (buffer-substring-no-properties
                       (point)
                       (line-beginning-position 4))))
      (with-temp-buffer
        (lisp-mode)
        (insert text-before)
        (save-excursion (insert text-after))
        (funcall fn)))))

;;; If point is in a comment, then call FN in in temporary buffer with
;;; all consecutive comment lines uncommented and point position at
;;; the original position. Else just call FN.
(defun mgl-pax-call-uncommented (fn)
  (let ((comment-bounds (mgl-pax-comment-lines-bounds)))
    (if comment-bounds
        (let ((comment (apply #'buffer-substring comment-bounds))
              (pos (1+ (- (point) (car comment-bounds)))))
          (with-temp-buffer
            (lisp-mode)
            (insert comment)
            (goto-char pos)
            (uncomment-region (point-min) (point-max))
            (funcall fn)))
      (funcall fn))))

(defun mgl-pax-comment-lines-bounds ()
  (when (elt (syntax-ppss) 4)
    (let* ((end (save-excursion
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t)
                  (comment-forward (point-max))
                  (point)))
           (beg (save-excursion
                  (forward-line 0)
                  (while (and (not (bobp))
                              (= end (save-excursion
                                       (comment-forward (point-max))
                                       (point))))
                    (forward-line -1))
                  (goto-char (line-end-position))
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t)
                  (ignore-errors
                    (while (looking-at-p comment-start-skip)
                      (forward-char -1)))
                  (point))))
      (list beg end))))

;;; Return the sexps before and after (slime-symbol-at-point),
;;; skipping some markup.
(cl-defun mgl-pax-find-locatives
    (&optional (bounds (slime-bounds-of-symbol-at-point)))
  (remove nil (list (mgl-pax-locative-before (car bounds))
                    (mgl-pax-locative-after (cdr bounds)))))

(cl-defun mgl-pax-locative-before (&optional (point (point)))
  (save-excursion
    (goto-char (1- point))
    (unless (looking-at "(")
      (skip-chars-backward "            ;` \n\t")
                          (let ((sexp (ignore-errors (slime-last-expression))))
                            (unless (equal sexp "")
                              sexp)))))

(cl-defun mgl-pax-locative-after (&optional (point (point)))
  (save-excursion
    (goto-char point)
    (skip-chars-forward "[];`\" \n\t")
    (if (and (char-after)
             (equal (string (char-after)) "("))
        ;; [FOO][(function)]
        (mgl-pax-next-sexp)
      ;; [FOO][function], [`FOO`][function], [FOO ][function]
      (let ((end-pos+1 (save-excursion
                         (search-forward-regexp "\\(\\]\\|`\\)"
                                                (+ (point) 1000)
                                                t))))
        (if end-pos+1
            (save-restriction
              (narrow-to-region (point) (1- end-pos+1))
              (mgl-pax-next-sexp))
          (mgl-pax-next-sexp))))))

;;; Return the next sexp as a string or nil.
(defun mgl-pax-next-sexp ()
  (save-excursion
    (when (mgl-pax-forward-sexp)
      (ignore-errors (slime-last-expression)))))

;;; Like forward-sexp, but don't signal errors and return t if
;;; something other than whitespace was skipped over.
(defun mgl-pax-forward-sexp ()
  (let ((point (point)))
    (ignore-errors (forward-sexp))
    (save-excursion
      (ignore-errors (backward-sexp))
      (<= point (point)))))

;;; With point on FOO or just after, parse "[FOO][function]" as a
;;; Markdown reference link. Return the name and the locative string
;;; as a list like ("FOO" ("function")).
(cl-defun mgl-pax-parse-reflink
    (&optional (bounds (slime-bounds-of-symbol-at-point)))
  (when bounds
    (let ((wall ()))
      (cl-flet ((add (start end)
                     (let ((name (buffer-substring-no-properties start end))
                           (locative (mgl-pax-locative-after end)))
                       (push (list name (if locative
                                            (list locative)
                                          ()))
                             wall))))
        (cl-destructuring-bind (symbol-start . symbol-end) bounds
          (save-restriction
            ;; Do not search beyond the surrounding lines.
            (let* ((min (save-excursion (ignore-errors (previous-line))
                                        (beginning-of-line)
                                        (point)))
                   (max (save-excursion (ignore-errors (next-line))
                                        (end-of-line)
                                        (point)))
                   (start-pos (save-excursion (search-backward "[" min t)))
                   (end-pos (save-excursion (search-forward "]" max t))))
              (when (and start-pos end-pos)
                ;; Exclude the bracket characters.
                (let ((start-pos (save-excursion
                                   (goto-char (1+ start-pos))
                                   (skip-chars-forward " \t\n")
                                   (point)))
                      (end-pos (save-excursion (goto-char (1- end-pos))
                                               (skip-chars-backward " \t\n")
                                               (point))))
                  ;; [lambda lists][clhs]
                  (add start-pos end-pos)
                  ;; [see also][foo function], [FOO function][docstring]
                  (when (and (< symbol-start start-pos)
                             (< symbol-end end-pos))
                    (add start-pos symbol-end))))))))
      (reverse wall))))


;;;; Integration with `M-.' (`slime-edit-definition')

;;; When it's on `slime-edit-definition-hooks', `M-.' calls this
;;; function with (slime-symbol-at-point) as NAME.
(defun mgl-pax-edit-definitions (name &optional where)
  (if (slime-eval '(cl:and (cl:find-package :mgl-pax)
                           (cl:find-symbol (cl:string '#:*navigate-loaded*)
                                           :mgl-pax)
                           cl:t))
      ;; No autoloading will take place, and everything else is quite
      ;; quick. Do things synchronously.
      (mgl-pax-edit-definitions-1 name where nil)
    ;; Either mgl-pax or mgl-pax/navigate will be autoloaded, which
    ;; may be slow or fail to compile. Do things asynchronously.
    (mgl-pax-maybe-autoload
     :no-web
     (lambda (loadedp)
       ;; `slime-edit-definition' is functional even without PAX. Remain
       ;; silent if PAX is not loaded.
       (when loadedp
         (mgl-pax-edit-definitions-1 name where t))))
    ;; Return nil and let other hooks run. It's racy, but they will
    ;; probably visit the same source location as
    ;; mgl-pax-edit-definitions-1, and this branch is quite likely
    ;; taken only once (subject to a similar race).
    nil))

(defun mgl-pax-edit-definitions-1 (name where asyncp)
  (let ((name-in-buffer (slime-symbol-at-point)))
    (if (string= name name-in-buffer)
        (mgl-pax-edit-buffer-definitions asyncp)
      (mgl-pax-edit-interactive-definitions name where asyncp))))

(defun mgl-pax-edit-buffer-definitions (asyncp)
  (mgl-pax-locate-definitions (mgl-pax-wall-at-point)
                              'mgl-pax-visit-locations
                              asyncp))

(defun mgl-pax-edit-interactive-definitions (string where asyncp)
  (let ((pos (cl-position ?\s string)))
    (if pos
        (let ((first (cl-subseq string 0 pos))
              (second (cl-subseq string (1+ pos))))
          ;; "FOO function" or "function FOO"
          (mgl-pax-locate-definitions `((,first (,second))
                                        (,second (,first)))
                                      'mgl-pax-visit-locations
                                      asyncp))
      ;; "FOO"
      (mgl-pax-locate-definitions `((,string ()))
                                  'mgl-pax-visit-locations
                                  asyncp))))

(defun mgl-pax-locate-definitions (name-and-locatives-list cont asyncp)
  (let ((form `(cl:funcall (cl:find-symbol
                            (cl:string '#:locate-definitions-for-emacs)
                            :mgl-pax)
                           ',name-and-locatives-list)))
    (if asyncp
        (slime-eval-async form cont)
      (funcall cont (slime-eval form)))))

(defun mgl-pax-visit-locations (dspec-and-location-list)
  (when (consp dspec-and-location-list)
    (if (eq (car dspec-and-location-list) :error)
        (message "%s" (cl-second dspec-and-location-list))
      (slime-edit-definition-cont
       (slime-postprocess-xrefs dspec-and-location-list)
       "dummy name"
       nil))))

(add-hook 'slime-edit-definition-hooks 'mgl-pax-edit-definitions)


(defun mgl-pax-edit-for-cl (dspec-and-location-list)
  ;; There may be no lisp-mode buffer at all.
  (ignore-errors (slime-recently-visited-buffer 'lisp-mode))
  (mgl-pax-sync-current-buffer)
  (x-focus-frame nil)
  (raise-frame)
  (mgl-pax-visit-locations dspec-and-location-list))

(defun mgl-pax-sync-current-buffer ()
  ;; https://emacs.stackexchange.com/questions/10921/why-doesnt-changing-buffer-in-filter-function-have-any-effect-in-ert
  (set-buffer (window-buffer (selected-window))))


;;;; MGL-PAX documentation browser
;;;;
;;;; Like `C-h f` (describe-function) but for Common Lisp via PAX.

(make-variable-buffer-local
 (defvar mgl-pax-doc-dir nil))

(defvar mgl-pax-doc-buffers ())

(defun mgl-pax-require-w3m ()
  (when (mgl-pax-use-w3m)
    (unless (require 'w3m nil t)
      (error "PAX requires w3m but it cannot be loaded. Please install it."))))

(defun mgl-pax-document (pax-url)
  "Browse the documentation of CL definitions for PAX-URL.

The documentation is a single HTML page generated by PAX via
Slime documenting the definitions given by PAX-URL. If necessary,
a disambiguation page is generated with the documentation of all
possible references. The HTML page is opened in the w3m browser
within Emacs.

When invoked programatically, PAX-URL must be a properly
urlencoded string with URL scheme \"pax:\". The format of PAX-URL
is:

  URL = \"pax:\" [REFERENCE] [\"#\" FRAGMENT]

where REFERENCE names either

- a complete CL PAX:REFERENCE (e.g. \"PAX:SECTION CLASS\"),

- or the object of a reference (e.g. \"PAX:SECTION\"), which
  possibly makes what to document ambiguous.

If given, FRAGMENT must be a complete PAX:REFERENCE and refers to
a definition within the documentation page of REFERENCE. For
example, the URL \"pax::@pax-manual pax:section#pax:defsection
pax:macro\" points to the documentation of the DEFSECTION macro
on the page that contains the entire PAX manual.

When invoked interactively:

- Without a prefix arg, the object of the reference defaults to
  `slime-symbol-at-point' and an attempt is made to find the
  locative around that. This works in non-lisp buffers as well.

- With a prefix arg, when no object is found, or when there are
  no definitions found for the object and locatives found in the
  buffer, `mgl-pax-document' prompts for URL. In this case, the
  URL scheme, \"pax:\", must not to be included. The entered
  REFERENCE and FRAGMENT need not be URL encoded.

- When PAX-URL is the string \"pax:\" (e.g. when the empty string
  was entered interactively), an existing w3m buffer is
  displayed, or if there is no such buffer, then the
  documentation of how to browse documentation in Emacs is shown.

Autocomplete: In the minibuffer, TAB-completion is available for
symbol objects and once the object is entered followed by a space
also for their possible locatives. Only symbols are completed.
String objects are not (e.g names of PACKAGEs or CLHS SECTIONs).
Completion of locatives which are lists (e.g. `(CLHS SECTION)')
is a bit broken because Emacs completion is designed for symbols.
Still, pressing TAB before entering the opening parenthesis and
selecting the locative from the buffer that pops up works. For
finding all definitions with a given locative, use
`mgl-pax-apropos'.

The package in which symbols are read is `slime-current-package'.
Hence, in Lisp buffers, the buffer's package is used. In other
buffers, the package of the repl.

The suggested key binding is `C-.' to parallel `M-.'."
  (interactive (list nil))
  (slime-check-connected)
  (mgl-pax-require-w3m)
  ;; Handle the interactive defaults here because it involves async
  ;; calls.
  (mgl-pax-ensure-pax-loaded ()
    (cond (pax-url
           (mgl-pax-document-pax-url pax-url))
          ;; interactive with prefix arg
          (current-prefix-arg
           (mgl-pax-prompt-and-document))
          ;; interactive without prefix arg, point over a pax URL
          ((and (null current-prefix-arg)
                (mgl-pax-in-doc-buffer-p)
                (mgl-pax-doc-pax-url (w3m-anchor)))
           (mgl-pax-document-pax-url (mgl-pax-doc-pax-url (w3m-anchor))))
          ;; interactive without prefix arg, point not over a pax URL
          (t
           (let ((wall (mgl-pax-wall-at-point)))
             (if wall
                 (mgl-pax-document-pax-url
                  (concat "pax-wall:"
                          (url-hexify-string (format "%S" wall))))
               (mgl-pax-prompt-and-document)))))))

(cl-defmacro mgl-pax-with-nlx-barrier (&body body)
  `(catch 'nlx-barrier
     (let ((done nil))
       (unwind-protect
           (prog1 (progn ,@body)
             (setq done t))
         (unless done
           (throw 'nlx-barrier nil))))))

(defun mgl-pax-prompt-and-document ()
  ;; Cancel the non-local exits to avoid "error in process filter"
  ;; messages and the subsequent delay when this function is called by
  ;; `slime-async-eval' and `read-from-minibuffer' is C-g'ed.
  (mgl-pax-with-nlx-barrier
   (mgl-pax-document-pax-url
    (mgl-pax-urllike-to-url
     (mgl-pax-read-urllike-from-minibuffer "View Documentation of: ")))))

(defun mgl-pax-document-pax-url (pax-url)
  (if (mgl-pax-use-w3m)
      (mgl-pax-document-pax-url/w3m pax-url)
    (mgl-pax-document-pax-url/other pax-url)))

(defun mgl-pax-document-pax-url/other (pax-url)
  (let ((pax-url (if (string= pax-url "pax:")
                     (mgl-pax-make-pax-eval-url
                      '(mgl-pax::pax-document-home-page))
                   pax-url)))
    ;; Call mgl-pax-call-document-for-emacs with DIR nil to check for
    ;; errors (e.g. "no definition for xxx") before launching a
    ;; browser.
    (mgl-pax-call-document-for-emacs
     pax-url nil
     :ok-cont (lambda (url)
                (if (null url)
                    (mgl-pax-prompt-and-document)
                  (message nil)
                  (let ((url (concat mgl-pax-web-server-base-url "/"
                                     (url-hexify-string
                                      pax-url mgl-pax-url-allowed-chars)
                                     (when (slime-current-package)
                                       (concat "?pkg="
                                               (url-hexify-string
                                                (slime-current-package)))))))
                    (funcall (or mgl-pax-browser-function
                                 browse-url-browser-function)
                             url)))))))

;;; What characters need no escaping when PAX URLs are encoded in a
;;; URL path. This does not allow ?/ to keep relative links working.
(defconst mgl-pax-url-allowed-chars
  (let ((vec (copy-sequence url-path-allowed-chars)))
    (aset vec ?/ nil)
    vec))

(defun mgl-pax-document-pax-url/w3m (pax-url)
  (unless (and (mgl-pax-in-doc-buffer-p) (string= pax-url "pax:"))
    (if (mgl-pax-in-doc-buffer-p)
        ;; When "entering" the documentation browser, reload (see
        ;; `mgl-pax-doc-reload').
        (w3m-goto-url pax-url :reload)
      (let* ((doc-buffer (if (mgl-pax-in-doc-buffer-p)
                             (current-buffer)
                           (cl-first mgl-pax-doc-buffers))))
        (if doc-buffer
            (if (string= pax-url "pax:")
                ;; Just show the latest doc buffer if the input is the
                ;; empty string.
                (pop-to-buffer doc-buffer)
              ;; Reuse doc-buffer and its doc dir.
              (let ((doc-dir (buffer-local-value 'mgl-pax-doc-dir doc-buffer)))
                (mgl-pax-call-document-for-emacs
                 pax-url doc-dir
                 :ok-cont (lambda (url)
                            (if (null url)
                                (mgl-pax-prompt-and-document)
                              ;; Maybe pop to a pax doc buffer.
                              (when (and (not (mgl-pax-in-doc-buffer-p))
                                         mgl-pax-doc-buffers)
                                (let ((package (slime-current-package)))
                                  (pop-to-buffer (cl-first mgl-pax-doc-buffers))
                                  (setq slime-buffer-package package)))
                              (w3m-goto-url url :reload)
                              (mgl-pax-set-up-doc-buffer doc-dir))))))
          ;; Display the docs of this very documentation browser if
          ;; the input is the empty string.
          (when (string= pax-url "pax:")
            (setq pax-url (mgl-pax-make-pax-eval-url
                           '(mgl-pax::pax-document-home-page))))
          ;; No doc buffer. Create a new dir.
          (let ((doc-dir (file-name-as-directory (make-temp-file "pax-doc" t))))
            (mgl-pax-call-document-for-emacs
             pax-url doc-dir
             :ok-cont (lambda (url)
                        (if (null url)
                            (mgl-pax-prompt-and-document)
                          (let ((package (slime-current-package)))
                            (w3m-goto-url url :reload)
                            (setq slime-buffer-package package))
                          (mgl-pax-set-up-doc-buffer doc-dir)))
             :abort-cont (lambda (condition)
                           (mgl-pax-delete-doc-dir doc-dir)))))))))

(defun mgl-pax-in-doc-buffer-p ()
  (buffer-local-value 'mgl-pax-doc-dir (current-buffer)))

(defun mgl-pax-set-up-doc-buffer (doc-dir)
  (setq mgl-pax-doc-dir doc-dir)
  (push (current-buffer) mgl-pax-doc-buffers)
  (add-hook 'kill-buffer-hook 'mgl-pax-tear-down-doc-buffer nil :local)
  (mgl-pax-doc-set-up-key-bindings))

(defun mgl-pax-doc-set-up-key-bindings ()
  (use-local-map (copy-keymap w3m-mode-map))
  ;; `M-.' visits the source when pressed on a "pax:" link.
  (local-set-key (kbd "M-.") 'slime-edit-definition)
  (local-set-key (kbd "M-,") 'slime-pop-find-definition-stack)
  (local-set-key (kbd "C-c C-d") 'slime-doc-map)
  ;; Make reloading regenerate the documentation.
  (local-set-key (kbd "R") 'mgl-pax-doc-reload)
  (local-set-key (kbd "n") 'mgl-pax-doc-next-definition)
  (local-set-key (kbd "p") 'mgl-pax-doc-previous-definition)
  (local-set-key (kbd "u") 'mgl-pax-doc-up-definition)
  (local-set-key (kbd "U") 'mgl-pax-doc-up-definition-and-beginning-of-buffer)
  (local-set-key (kbd "v") 'mgl-pax-doc-edit-current-definition)
  (local-set-key (kbd "V") 'mgl-pax-doc-edit-first-definition))

(defun mgl-pax-tear-down-doc-buffer ()
  (mgl-pax-delete-doc-dir)
  (setq mgl-pax-doc-buffers (remove (current-buffer) mgl-pax-doc-buffers)))

(cl-defun mgl-pax-delete-doc-dir (&optional (doc-dir mgl-pax-doc-dir))
  (when doc-dir
    ;; This could be a simple recursive delete call to
    ;; delete-directory, but this is less dangerous.
    (dolist (file (file-expand-wildcards (concat doc-dir "*.html")))
      (delete-file file nil))
    (ignore-errors
      (delete-directory doc-dir nil nil))))

(defun mgl-pax-urllike-to-url (schemeless-pax-url)
  (cl-destructuring-bind (reference fragment)
      (mgl-pax-parse-path-and-fragment schemeless-pax-url)
    (if fragment
        (concat "pax:" (url-hexify-string reference)
                "#" (url-hexify-string fragment))
      (concat "pax:" (url-hexify-string reference)))))

(defun mgl-pax-read-urllike-from-minibuffer (prompt)
  (let ((slime-completion-at-point-functions
         '(mgl-pax-complete-urllike-in-minibuffer)))
    (slime-read-from-minibuffer prompt)))

(defun mgl-pax-complete-urllike-in-minibuffer ()
  (let* ((start
          ;; The position of the first character after the prompt
          (line-beginning-position -1000))
         (end (point))
         (beg (slime-symbol-start-pos))
         (first-space-pos (cl-position ?\s (buffer-substring-no-properties
                                            start end))))
    (if first-space-pos
        (let ((object (buffer-substring-no-properties
                       start (+ start first-space-pos))))
          (list beg end (completion-table-dynamic
                         (lambda (prefix)
                           (mgl-pax-locatives-for-name object)))))
      (list beg end (completion-table-dynamic #'slime-simple-completions)))))

(defun mgl-pax-locatives-for-name (object)
  (let ((values (slime-eval
                 `(cl:funcall (cl:find-symbol
                               (cl:string '#:locatives-for-name-for-emacs)
                               :mgl-pax)
                              ,object))))
    (if (eq (cl-first values) :error)
        (error (second values))
      (cl-second values))))

;;; Return the path and fragment part of the schemeless URL.
(defun mgl-pax-parse-path-and-fragment (url)
  (let ((fragment-pos (cl-position ?# url)))
    (if fragment-pos
        (list (cl-subseq url 0 fragment-pos)
              (cl-subseq url (1+ fragment-pos)))
      (list url nil))))

(defun mgl-pax-w3m-goto-url (oldfun url &rest args)
  (if (not (or (string-prefix-p "pax:" url)
               (string-prefix-p "pax-eval:" url)
               (string-prefix-p "pax-wall:" url)))
      (apply oldfun url args)
    ;; Set up pax e.g. when the user starts w3m, then presses g and
    ;; enters a pax url.
    (unless (mgl-pax-in-doc-buffer-p)
      (mgl-pax-set-up-doc-buffer
       (file-name-as-directory (make-temp-file "pax-doc" t))))
    (let ((buffer (current-buffer)))
      (mgl-pax-call-document-for-emacs url mgl-pax-doc-dir
                                       :ok-cont
                                       (lambda (url)
                                         (when url
                                           (pop-to-buffer buffer)
                                           (apply oldfun url args)))))))

;;; Like slime-eval-async, but call abort-cont on :abort.
(defun mgl-pax-eval-async (sexp ok-cont &optional abort-cont package)
  (slime-rex (ok-cont abort-cont (buffer (current-buffer)))
      (sexp (or package (slime-current-package)))
    ((:ok result)
     (when ok-cont
       (set-buffer buffer)
       (funcall ok-cont result)))
    ((:abort condition)
     (if abort-cont
         (funcall abort-cont condition)
       (message "Evaluation aborted on %s." condition)))))

(cl-defun mgl-pax-call-document-for-emacs (url dir &key ok-cont abort-cont)
  (mgl-pax-eval-async
   `(cl:funcall (cl:find-symbol (cl:string '#:document-for-emacs)
                                :mgl-pax)
                ',url ',dir ',common-lisp-hyperspec-root)
   (lambda (values)
     (if (eq (cl-first values) :url)
         (apply ok-cont (cl-rest values))
       (message "%s" (cl-second values))
       (when abort-cont
         (apply abort-cont (cl-rest values)))))
   abort-cont)
  (message "Generating documentation ..."))

(advice-add 'w3m-goto-url :around #'mgl-pax-w3m-goto-url)

(defun mgl-pax-doc-reload ()
  "Like `w3m-reload-this-page', but also regenerate the documentation
if the current page was generated from a PAX URL."
  (interactive)
  (when mgl-pax-doc-dir
    (let ((buffer (current-buffer)))
      (mgl-pax-call-redocument-for-emacs w3m-current-url mgl-pax-doc-dir
                                         (lambda ()
                                           (pop-to-buffer buffer)
                                           (w3m-reload-this-page))))))

(defun mgl-pax-call-redocument-for-emacs (file-url dir cont)
  (slime-eval-async
      `(cl:if (cl:find-package :mgl-pax)
              (cl:funcall (cl:find-symbol (cl:string '#:redocument-for-emacs)
                                          :mgl-pax)
                          ',file-url ',dir ',common-lisp-hyperspec-root)
              '(:error "MGL-PAX is not loaded."))
    (lambda (values)
      (if (eq (cl-first values) :error)
          (message "%s" (cl-second values))
        (apply cont (cl-rest values)))))
  (message "Generating documentation ..."))


;;;; Navigation commands for PAX doc.
;;;;
;;;; These jump between the HTML anchors (<a id="...">) generated by
;;;; PAX before definitions (e.g. function signature lines, SECTION
;;;; titles).

(defun mgl-pax-doc-next-definition ()
  "Move point to the next PAX definition.
Use it in a PAX doc buffer (see `mgl-pax-document')."
  (interactive)
  (let ((start (mgl-pax-doc-definition-start)))
    (if (and start (< (point) start))
        (goto-char start)
      (let ((next (mgl-pax-doc-next-definition-start)))
        (if next
            (goto-char next)
          (unless start
            ;; There are no PAX definitions at all. Just move to the
            ;; next link.
            (w3m-next-anchor)))))))

(defun mgl-pax-doc-previous-definition ()
  "Move point to the previous PAX definition.
Use it in a PAX doc buffer (see `mgl-pax-document')."
  (interactive)
  (let ((start (mgl-pax-doc-definition-start)))
    (if (and start (< start (point)))
        (goto-char start)
      (let ((prev (mgl-pax-doc-prev-definition-start)))
        (if prev
            (goto-char prev)
          (unless start
            (w3m-previous-anchor)))))))

;;; Return the buffer position of the first character of the link
;;; corresponding to the current definition.
(defun mgl-pax-doc-definition-start ()
  (mgl-pax-definition-link-pos
   (previous-single-property-change (if (< (point) (buffer-size))
                                        (1+ (point))
                                      (point))
                                    'w3m-name-anchor2)))

(defun mgl-pax-doc-next-definition-start ()
  (mgl-pax-definition-link-pos
   (next-single-property-change (point) 'w3m-name-anchor2)))

(defun mgl-pax-doc-prev-definition-start ()
  (let ((this (previous-single-property-change (if (< (point) (buffer-size))
                                                   (1+ (point))
                                                 (point))
                                               'w3m-name-anchor2)))
    (when this
      (mgl-pax-definition-link-pos
       (previous-single-property-change this 'w3m-name-anchor2)))))

(defun mgl-pax-definition-link-pos (pos)
  (when pos
    (save-excursion
      (goto-char pos)
      (unless (w3m-anchor)
        (w3m-next-anchor))
      (point))))

(defun mgl-pax-doc-up-definition ()
  "Follow the first \"Up:\" link at the top of the PAX documentation if any.

That is, in a PAX doc buffer (see `mgl-pax-document'), open a new
URL with the documentation of the first containing section and
put point on the definition corresponding the current page.

When there multiple sections that contain the current object, the
first one will be chosen heuristically based on the similarity of
the names of the SYMBOL-PACKAGEs of their names."
  (interactive)
  (let ((url (mgl-pax-doc-url-up)))
    (when url
      (w3m-goto-url url)
      t)))

(defun mgl-pax-doc-up-definition-and-beginning-of-buffer ()
  "Like `mgl-pax-doc-up-definition', but also move point to
the beginning of the buffer. If there is no \"Up:\" link, then
move point to the beginning of the buffer."
  (interactive)
  (let ((url (mgl-pax-doc-url-up t)))
    (if (null url)
        (beginning-of-buffer)
      (w3m-goto-url url)
      t)))

(defun mgl-pax-doc-url-up (&optional strip-fragment-p)
  (when (mgl-pax-doc-has-up-line-p)
    (save-excursion
      (beginning-of-buffer)
      (w3m-next-anchor)
      (let ((url (w3m-anchor)))
        (when url
          (if strip-fragment-p
              (w3m-url-strip-fragment url)
            url))))))

(defun mgl-pax-doc-has-up-line-p ()
  (save-excursion
    (beginning-of-buffer)
    (forward-line)
    (and (<= (+ (point) 4) (buffer-size))
         (string= (buffer-substring-no-properties (point) (+ (point) 4))
                  "Up: "))))

(defun mgl-pax-doc-edit-current-definition ()
  "Visit the source of the current PAX definition on the page."
  (interactive)
  (mgl-pax-doc-edit-pax-definition
   (or (mgl-pax-doc-current-definition-pax-url)
       ;; There is always a current definition unless the point is
       ;; before the first definition, so default to that.
       (mgl-pax-doc-first-definition-pax-url))))

(defun mgl-pax-doc-current-definition-pax-url ()
  (let ((pos (mgl-pax-doc-definition-start)))
    (when pos
      (save-excursion
        (goto-char pos)
        (mgl-pax-doc-pax-url (w3m-anchor))))))

(defun mgl-pax-doc-edit-first-definition ()
  "Visit the source of the first PAX definition on the page."
  (interactive)
  (mgl-pax-doc-edit-pax-definition (mgl-pax-doc-first-definition-pax-url)))

(defun mgl-pax-doc-first-definition-pax-url ()
  (save-excursion
    (beginning-of-buffer)
    (mgl-pax-doc-next-definition)
    (mgl-pax-doc-pax-url (w3m-anchor))))


;;;; Make `M-.' (`slime-edit-definition') work on links in w3m PAX
;;;; doc.

;;; If over a link in a w3m buffer, then visit the source if it is a
;;; "pax:" or "file:" URL, else do nothing. For "pax:" URLs, the URL
;;; itself identifies the target. For "file:" URLs, the target is the
;;; PAX reference encoded in the fragment part of the URL if any.
(defun mgl-pax-doc-edit-definition (name &optional where)
  (let ((url (and (fboundp 'w3m-anchor)
                  (mgl-pax-doc-pax-url (w3m-anchor)))))
    (mgl-pax-doc-edit-pax-definition url)))

(defun mgl-pax-doc-edit-pax-definition (pax-url)
  (when (string-prefix-p "pax:" pax-url)
    (slime-eval-async
        ;; Silently fail if MGL-PAX is not loaded.
        `(cl:when (cl:find-package :mgl-pax)
                  (cl:funcall
                   (cl:find-symbol (cl:string '#:locate-pax-url-for-emacs)
                                   :mgl-pax)
                   ',pax-url))
      'mgl-pax-visit-locations)))

(defun mgl-pax-doc-pax-url (url)
  (cond ((string-prefix-p "pax:" url)
         url)
        ((string-prefix-p "file:" url)
         (let ((fragment (elt (mgl-pax-parse-path-and-fragment url) 1)))
           (when fragment
             (concat "pax:" fragment))))))

(add-hook 'slime-edit-definition-hooks 'mgl-pax-doc-edit-definition)


;;;; Determining the current definition

(defun mgl-pax-current-definition-possible-names ()
  (save-excursion
    (when (looking-at "(")
      (ignore-errors (down-list)))
    (cl-loop for name-snippet-and-pos
             = (mgl-pax-current-sexp-first-arg-snippet-and-pos)
             when name-snippet-and-pos
             collect name-snippet-and-pos
             while (ignore-errors (backward-up-list 1 t t)
                                  t))))

;;; Return 1. the first argument of the current sexp if it's a symbol,
;;; 2. the Slime source location :SNIPPET, 3. the start position of
;;; the sexp. If any movement fails or the first argument is not a
;;; symbol, then return nil.
(defun mgl-pax-current-sexp-first-arg-snippet-and-pos ()
  (ignore-errors
    (save-excursion
      (backward-up-list 1 t t)
      (let ((snippet (mgl-pax-next-sexp))
            (pos (point)))
        (when (< 200 (length snippet))
          (setq snippet (cl-subseq snippet 0 200)))
        (down-list)
        (slime-forward-sexp)
        (forward-char)
        ;; `name' can be a symbol or a string ...
        (let ((name (mgl-pax-next-sexp)))
          ;; ... but currently never a list.
          (unless (string-prefix-p "(" name)
            (list name snippet pos)))))))


(defun mgl-pax-current-definition-toggle-view ()
  "Document the definition `point' is in with `mgl-pax-document'.
In a PAX doc buffer, it's equivalent to pressing `v'
(`mgl-pax-doc-edit-current-definition')."
  (interactive)
  (if (mgl-pax-in-doc-buffer-p)
      (mgl-pax-doc-edit-current-definition)
    (mgl-pax-ensure-pax-loaded ()
      (mgl-pax-current-definition-pax-url 'mgl-pax-document))))

(defun mgl-pax-current-definition-pax-url (cont)
  (slime-eval-async
      `(cl:if (cl:find-package :mgl-pax)
              (cl:funcall
               (cl:find-symbol (cl:string
                                '#:current-definition-pax-url-for-emacs)
                               :mgl-pax)
               ',(buffer-name)
               ',(buffer-file-name)
               ',(mgl-pax-current-definition-possible-names))
              '(:error "MGL-PAX is not loaded."))
    (lambda (values)
      (if (eq (cl-first values) :error)
          (message "%s" (cl-second values))
        (apply cont (cl-rest values))))))


(defun mgl-pax-edit-parent-section ()
  "Look up the definition of parent section of the definition
`point' is in as if with `M-.' (`slime-edit-definition'). If
there are multiple containing sections, then pop up a selection
buffer."
  (interactive)
  (mgl-pax-ensure-pax-loaded (:no-web t)
    (mgl-pax-find-parent-section #'mgl-pax-visit-locations)))

(defun mgl-pax-find-parent-section (cont)
  (slime-eval-async
      `(cl:if (cl:find-package :mgl-pax)
              (cl:funcall
               (cl:find-symbol (cl:string
                                '#:find-parent-section-for-emacs)
                               :mgl-pax)
               ',(buffer-name)
               ',(buffer-file-name)
               ',(mgl-pax-current-definition-possible-names))
              '(:error "MGL-PAX is not loaded."))
    cont))


;;;; Apropos

(defun mgl-pax-apropos (string &optional external-only package
                               case-sensitive)
  "Show all PAX definitions that match the arguments.
This is a wrapper around DREF:DREF-APROPOS. STRING is basically
NAME and LOCATIVE-TYPES concatenated with a space in between. If
STRING or PACKAGE starts with `?'', then only exact matches with
a symbol or package name are accepted.

- \"print\" matches definitions whose names contain \"print\" as
  a substring.

- \"'print\" matches definitions whose names are \"print\" (still
  subject to CASE-SENSITIVE).

- \"print function\" matches functions whose names contain
  \"print\" (e.g. CL:PRINT and CL:PPRINT).

- \"'print function\" is like the previous example but with exact
  name match.

- \"print variable\" matches for example *PRINT-ESCAPE*.

- \"print variable function\" matches all variables and functions
  with \"print\" in their names.

- \" pax:section\" (note the leading space) matches all PAX
  sections (note that EXTERNAL-ONLY NIL is necessary to see most
  of them).

- \"print :lisp\" matches definitions with
  DREF:LISP-LOCATIVE-TYPES, which is the default.

- \"print :pseudo\" matches definitions with
  DREF:PSEUDO-LOCATIVE-TYPES such as PAX:CLHS and DREF:UNKNOWN.

- \"print :all\" matches definitions with all locative
  types (DREF:LOCATIVE-TYPES).

With a prefix arg, you're interactively asked for parameters of
the search. Without a prefix arg, EXTERNAL-ONLY defaults to T,
packages and locative types are not filtered, and case does not
matter.

Also, see `mgl-pax-apropos-all'."
  (interactive (list nil nil nil nil))
  (mgl-pax-ensure-pax-loaded ()
    (mgl-pax-with-nlx-barrier
     (mgl-pax-document
      (mgl-pax-make-pax-eval-url
       (if string
           `(mgl-pax::pax-apropos* ,string ,external-only
                                   ,package ,case-sensitive)
         `(mgl-pax::pax-apropos*
           ;; Do the defaulting of arguments here instead of in
           ;; `interactive' because
           ;; `mgl-pax-read-urllike-from-minibuffer' relies on
           ;; `mgl-pax-ensure-pax-loaded' having succeeded.
           ,@(if current-prefix-arg
                 (list (mgl-pax-read-urllike-from-minibuffer
                        "PAX Apropos: ")
                       (y-or-n-p "External symbols only? ")
                       (slime-read-package-name "Package: ")
                       (y-or-n-p "Case-sensitive? "))
               (list (mgl-pax-read-urllike-from-minibuffer
                      "PAX Apropos: ")
                     t nil nil)))))))))

(defun mgl-pax-make-pax-eval-url (sexp)
  (concat "pax-eval:" (url-encode-url (prin1-to-string sexp))))

(defun mgl-pax-apropos-all (string)
  "Shortcut for invoking `mgl-pax-apropos' with EXTERNAL-ONLY NIL."
  (interactive (list nil))
  (mgl-pax-ensure-pax-loaded ()
    (mgl-pax-with-nlx-barrier
     (let ((string (or string (mgl-pax-read-urllike-from-minibuffer
                               "PAX Apropos All: "))))
       (mgl-pax-apropos string nil "" nil)))))

(defun mgl-pax-apropos-package (package &optional internal)
  "Show apropos listing for symbols in PACKAGE.
With prefix argument include internal symbols.
The empty string means the current package."
  (interactive (list (let ((pkg (slime-read-package-name
                                 "PAX Apropos for Package: ")))
                       (if (string= pkg "") (slime-current-package) pkg))
                     current-prefix-arg))
  (mgl-pax-apropos "" (not internal) (concat "'" package) nil))


;;;; Transcribe

(defun mgl-pax-transcribe-last-expression ()
  "A bit like C-u C-x C-e (slime-eval-last-expression) that
inserts the output and values of the sexp before the point, this
does the same but with MGL-PAX:TRANSCRIBE. Use a numeric prefix
argument as in index to select one of the Common Lisp
MGL-PAX:*SYNTAXES* as the SYNTAX argument to MGL-PAX:TRANSCRIBE.
Without a prefix argument, the first syntax is used."
  (interactive)
  (mgl-pax-ensure-pax-loaded (:no-web t)
    (save-excursion
      (let ((dynenv (mgl-pax-find-cl-transcript-dynenv)))
        (let* ((start (progn (backward-sexp)
                             ;; If the last expression is in a
                             ;; comment, we need this for
                             ;; forward-sexp below.
                             (save-excursion
                               (move-beginning-of-line nil)
                               (point))))
               (end (progn (forward-sexp)
                           (point))))
          (goto-char end)
          (insert
           (mgl-pax-transcribe start end (mgl-pax-transcribe-syntax-arg)
                               nil nil nil dynenv))
          ;; The transcript ends with a newline. Delete it if it
          ;; would result in a blank line.
          (when (looking-at "\n")
            (delete-char 1)))))))

(defun mgl-pax-retranscribe-region (start end)
  "Updates the transcription in the current region (as in calling
MGL-PAX:TRANSCRIBE with :UPDATE-ONLY T). Use a numeric prefix
argument as an index to select one of the Common Lisp
MGL-PAX:*TRANSCRIBE-SYNTAXES* as the SYNTAX argument to
MGL-PAX:TRANSCRIBE. Without a prefix argument, the syntax of the
input will not be changed."
  (interactive "r")
  (mgl-pax-ensure-pax-loaded (:no-web t)
    (let ((dynenv (mgl-pax-find-cl-transcript-dynenv)))
      (let* ((point-at-start-p (= (point) start))
             (point-at-end-p (= (point) end))
             (transcript (mgl-pax-transcribe start end
                                             (mgl-pax-transcribe-syntax-arg)
                                             t t nil dynenv)))
        (if point-at-start-p
            (save-excursion
              (goto-char start)
              (delete-region start end)
              (insert transcript))
          (save-excursion
            (goto-char start)
            (delete-region start end))
          (insert transcript))))))

(defun mgl-pax-transcribe-syntax-arg ()
  (if current-prefix-arg
      (prefix-numeric-value current-prefix-arg)
    nil))

;;; Within the current defun, find the first occurrence of "```"
;;; backwards from point, and if it is followed by "cl-transcript",
;;; return its dynenv argument."
(defun mgl-pax-find-cl-transcript-dynenv ()
  (save-excursion
    (save-restriction
      (narrow-to-defun)
      (when (search-backward "```" nil t)
        (when (looking-at "```cl-transcript")
          (save-restriction
            (narrow-to-region (point) (save-excursion
                                        (end-of-line)
                                        (point)))
            (when (search-forward ":dynenv" nil t)
              (mgl-pax-next-sexp))))))))

(defun mgl-pax-transcribe (start end syntax update-only echo
                                 first-line-special-p dynenv)
  (slime-eval
   `(cl:funcall (cl:find-symbol (cl:string '#:transcribe-for-emacs) :mgl-pax)
                ,(buffer-substring-no-properties start end)
                ',syntax ',update-only ',echo ',first-line-special-p ,dynenv)))

(provide 'mgl-pax)
