;; -*- lexical-binding: t -*-

;;;; MGL-PAX Emacs integration of navigating, documenting, and
;;;; transcribing
;;;;
;;;; - Making `M-.' (`slime-edit-definition') support new kinds of
;;;;   definitions (e.g. of ASDF/SYSTEMs) and disambiguate based on
;;;;   nearby locatives. Just by loading this file, `M-.' shall be
;;;;   able to recognize disambiguate based on locatives near point as
;;;;   in "function FOO". See the documentation in
;;;;   MGL-PAX::@NAVIGATING-IN-EMACS.
;;;;
;;;; - Browsing documentation of definitions in the running Lisp live
;;;;   without explicitly generating documentation. This is what
;;;;   `mgl-pax-document' does. Bind it to `C-.' to parallel `M-.'.
;;;;   See the documentation in MGL-PAX::@DOCUMENTING-IN-EMACS (e.g.
;;;;   by pressing `C-.' with point over this section name).
;;;;
;;;; - `mgl-pax-apropos', `mgl-pax-apropos-all' and
;;;;   `mgl-pax-apropos-package' are replacements for `slime-apropos'
;;;;   `slime-apropos-all' and `slime-apropos-package', respectively.
;;;;   They are built on top of `mgl-pax-document'.
;;;;
;;;; - For `mgl-pax-transcribe-last-expression' and
;;;;   `mgl-pax-retranscribe-region', see
;;;;   MGL-PAX::@TRANSCRIBING-WITH-EMACS.

(eval-and-compile
  (require 'cl-lib nil t)
  ;; For emacs 23, look for bundled version
  (require 'cl-lib "lib/cl-lib")
  (require 'slime))


(defun mgl-pax-hijack-slime-doc-keys ()
  "If `w3m' is available, replace `slime-apropos',
`slime-apropos-all', `slime-apropos-package' with
`mgl-pax-apropos', `mgl-pax-apropos-all',
`mgl-pax-apropos-package', and replace both
`slime-describe-symbol' and `slime-describe-function' with
`mgl-pax-document'.

In addition, because it can be almost as useful as `M-.', one may
want to give `mgl-pax-document' a more convenient binding such as
`C-.' or `s-.' if you have a Super key. For example, to bind
`C-.' in all Slime buffers:

    (slime-bind-keys slime-parent-map nil '((\"C-.\" mgl-pax-document)))

To bind `C-.' globally:

    (global-set-key (kbd \"C-.\") 'mgl-pax-document)"
  (if (not (require 'w3m nil t))
      (message "Requiring w3m failed.")
    (slime-bind-keys slime-doc-map t
                     '((?a mgl-pax-apropos)
                       (?z mgl-pax-apropos-all)
                       (?p mgl-pax-apropos-package)
                       (?d mgl-pax-document)
                       (?f mgl-pax-document)))))


;;;; Autoloading of MGL-PAX on the Common Lisp side

(defcustom mgl-pax-autoload t
  "If true, then the MGL-PAX ASDF system will be loaded as necessary
via Slime by `slime-edit-definition', `mgl-pax-document' and
other mgl-pax commands."
  :type 'boolean
  :group 'mgl-pax)

(defvar mgl-pax-version '(0 2 0))

(defun mgl-pax-maybe-autoload (cont)
  (if mgl-pax-autoload
      (slime-eval-async
          `(cl:progn
            (cl:unless (cl:find-package :mgl-pax)
                       (cl:format t ";; Autoloading MGL-PAX for Emacs ~
                                     (mgl-pax-autoload is t).~%")
                       (asdf:load-system "mgl-pax")
                       (cl:format t ";; Done autoloading MGL-PAX for Emacs~%"))
            (cl:and (cl:find-package :mgl-pax)
                    (cl:funcall (cl:find-symbol
                                 (cl:string '#:check-pax-elisp-version)
                                 (cl:find-package :mgl-pax))
                                ',mgl-pax-version)
                    t))
        cont)
    (slime-eval-async
        `(cl:and (cl:find-package :mgl-pax)
                 (cl:funcall (cl:find-symbol
                              (cl:string '#:check-pax-elisp-version)
                              (cl:find-package :mgl-pax))
                             ',mgl-pax-version)
                 t)
      cont)))


;;;; Find possible objects and locatives at point

;;; Return a list of of things like (object (locative1 locative2 ...))
;;; representing the possible references (object locative1), (object
;;; locative2), and so on. MGL-PAX::LOCATE-DEFINITION-FOR-EMACS and
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
(defun mgl-pax-object-and-locatives-list-at-point ()
  (let* ((name (slime-symbol-at-point))
         (bounds (slime-bounds-of-symbol-at-point))
         (locatives (mgl-pax-find-locatives bounds))
         (reflink-name-and-locatives (mgl-pax-parse-reflink bounds)))
    (remove nil `((,name ,locatives)
                  ;; This may be nil.
                  ,reflink-name-and-locatives))))

;;; Return the sexps before and after (slime-symbol-at-point),
;;; skipping some markup.
(cl-defun mgl-pax-find-locatives
    (&optional (bounds (slime-bounds-of-symbol-at-point)))
  (remove nil (list (mgl-pax-locative-before (car bounds))
                    (mgl-pax-locative-after (cdr bounds)))))

(cl-defun mgl-pax-locative-before (&optional (point (point)))
  (ignore-errors
    (save-excursion
      (goto-char (1- point))
      (skip-chars-backward ";` \n\t")
      (slime-last-expression))))

(cl-defun mgl-pax-locative-after (&optional (point (point)))
  (ignore-errors
    (save-excursion
      (goto-char point)
      (skip-chars-forward "[];`\" \n\t")
      (if (equal (string (char-after)) "(")
          ;; [FOO][(function)]
          (save-excursion
            (slime-forward-sexp)
            (slime-last-expression))
        ;; [FOO][function], [`FOO`][function], [FOO ][function]
        (let ((end-pos+1 (save-excursion
                           (search-forward-regexp "\\(\\]\\|`\\)"
                                                  (+ (point) 1000)
                                                  t))))
          (if end-pos+1
              (save-restriction
                (narrow-to-region (point) (1- end-pos+1))
                (slime-forward-sexp)
                (slime-last-expression))
            (slime-forward-sexp)
            (slime-last-expression)))))))

;;; With point on FOO or just after, parse "[FOO][function]" as a
;;; Markdown reference link. Return the name and the locative string
;;; as a list like ("FOO" ("function")).
(cl-defun mgl-pax-parse-reflink
    (&optional (bounds (slime-bounds-of-symbol-at-point)))
  (when bounds
    (cl-destructuring-bind (symbol-start . symbol-end) bounds
      (let ((ref-start (1+ (or (save-excursion
                                 (search-backward "[" symbol-start t))
                               (1- symbol-start))))
            (ref-end (1- (or (save-excursion
                               (search-forward "]" symbol-end t))
                             (1+ symbol-end)))))
        ;; If the name contains ?\[ or ?\] ...
        (unless (and (= ref-start symbol-start)
                     (= ref-end symbol-end))
          ;; ... then cut off anything beyond those characters to get a
          ;; new name.
          (let ((name (buffer-substring-no-properties ref-start ref-end))
                (locative (mgl-pax-locative-after ref-end)))
            (list name (if locative
                           (list locative)
                         ()))))))))


;;;; Integration with `M-.' (`slime-edit-definition')

;;; When it's on `slime-edit-definition-hooks', `M-.' calls this
;;; function with (slime-symbol-at-point) as NAME.
(defun mgl-pax-edit-definitions (name &optional where)
  (let ((name-in-buffer (slime-symbol-at-point)))
    (if (string= name name-in-buffer)
        (mgl-pax-edit-buffer-definitions)
      (mgl-pax-edit-interactive-definitions name where))))

(defun mgl-pax-edit-buffer-definitions ()
  (mgl-pax-locate-definitions (mgl-pax-object-and-locatives-list-at-point)
                              'mgl-pax-visit-locations))

(defun mgl-pax-edit-interactive-definitions (string &optional where)
  (let ((pos (cl-position ?\s string)))
    (if pos
        (let ((first (cl-subseq string 0 pos))
              (second (cl-subseq string (1+ pos))))
          ;; "FOO function" or "function FOO"
          (mgl-pax-locate-definitions `((,first (,second))
                                        (,second (,first)))
                                      'mgl-pax-visit-locations))
      ;; "FOO"
      (mgl-pax-locate-definitions `((,string ()))
                                  'mgl-pax-visit-locations))))

(cl-defun mgl-pax-locate-definitions (name-and-locatives-list cont &key as-ref)
  (mgl-pax-maybe-autoload
   (lambda (loadedp)
     (if (not loadedp)
         ;; Do not be annoying in the M-. case.
         (when as-ref
           (mgl-pax-not-loaded))
       (slime-eval-async
           `(cl:when (cl:find-package :mgl-pax)
                     (cl:funcall (cl:find-symbol
                                  (cl:string '#:locate-definitions-for-emacs)
                                  :mgl-pax)
                                 ',name-and-locatives-list
                                 :as-ref ,as-ref))
         cont)))))

(defun mgl-pax-not-loaded ()
  (message "MGL-PAX is not loaded. See the variable mgl-pax-autoload."))

(defun mgl-pax-visit-locations (locations)
  (when (consp locations)
    (if (eq (car locations) :error)
        (message "%s" (cl-second locations))
      (slime-edit-definition-cont
       (slime-postprocess-xrefs locations)
       "dummy name"
       nil))))

(add-hook 'slime-edit-definition-hooks 'mgl-pax-edit-definitions)


;;;; MGL-PAX documentation browser
;;;;
;;;; Like `C-h f` (describe-function) but for Common Lisp via PAX.

(make-variable-buffer-local
 (defvar mgl-pax-doc-dir nil))

(defvar mgl-pax-doc-buffers ())

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

Note that when invoked interactively, the URL scheme, \"pax:\",
must not to be included in PAX-URL and the entered REFERENCE and
FRAGMENT will be automatically urlencoded.

When PAX-URL is the string \"pax:\" (e.g. when the empty string
was entered interactively), an existing w3m buffer is displayed,
or if there is no such buffer, then the documentation of how to
browse documentation in Emacs is shown.

The suggested key binding is `C-.' to parallel `M-.'."
  (interactive (list nil))
  ;; Handle the interactive defaults here because it involves async
  ;; calls.
  (cond (pax-url
         (mgl-pax-document-pax-url pax-url))
        ;; interactive with prefix arg
        (current-prefix-arg
         (mgl-pax-prompt-and-document))
        ;; interactive without prefix arg, point over a pax URL
        ((and (mgl-pax-in-doc-buffer-p)
              (mgl-pax-doc-pax-url (w3m-anchor)))
         (mgl-pax-document-pax-url (mgl-pax-doc-pax-url (w3m-anchor))))
        ;; interactive without prefix arg, point not over a pax URL
        (t
         (mgl-pax-references-at-point
          (lambda (references)
            (cond ((eq (cl-first references) :error)
                   (message (cl-second references)))
                  ((cl-endp references)
                   (mgl-pax-prompt-and-document))
                  (t
                   (cl-destructuring-bind (object locative)
                       (cl-first references)
                     (mgl-pax-document-pax-url
                      (mgl-pax-urllike-to-url
                       (if (= (length references) 1)
                           (concat object " " locative)
                         ;; Go to the disambiguation page.
                         object)))))))))))

(defun mgl-pax-prompt-and-document ()
  (catch 'exit
    (mgl-pax-document-pax-url
     (mgl-pax-urllike-to-url
      (let ((done nil))
        (unwind-protect
            (prog1
                (slime-read-from-minibuffer "View Documentation of: ")
              (setq done t))
          (unless done
            ;; Cancel the non-local exit to avoid "error in process
            ;; filter" message and the subsequent delay when this
            ;; function is called by `slime-async-eval'.
            (throw 'exit nil))))))))

(defun mgl-pax-references-at-point (cont)
  (mgl-pax-locate-definitions (mgl-pax-object-and-locatives-list-at-point)
                              cont :as-ref t))

(defun mgl-pax-document-pax-url (pax-url)
  (unless (and (mgl-pax-in-doc-buffer-p) (string= pax-url "pax:"))
    (if (mgl-pax-in-doc-buffer-p)
        ;; When "entering" the documentation browser, reload (see
        ;; `mgl-pax-doc-reload').
        (w3m-goto-url pax-url :reload)
      (let* ((doc-buffer (if (mgl-pax-in-doc-buffer-p)
                             (current-buffer)
                           (cl-first mgl-pax-doc-buffers))))
        (if doc-buffer
            ;; Just show the latest doc buffer if the input is the
            ;; empty string.
            (if (string= pax-url "pax:")
                (pop-to-buffer doc-buffer)
              ;; Reuse doc-buffer and its doc dir.
              (let ((doc-dir (buffer-local-value 'mgl-pax-doc-dir doc-buffer)))
                (mgl-pax-call-document-for-emacs
                 pax-url doc-dir
                 :ok-cont (lambda (file-url)
                            ;; Maybe pop to a pax doc buffer.
                            (when (and (not (mgl-pax-in-doc-buffer-p))
                                       mgl-pax-doc-buffers)
                              (let ((package (slime-current-package)))
                                (pop-to-buffer (cl-first mgl-pax-doc-buffers))
                                (setq slime-buffer-package package)))
                            (w3m-goto-url file-url :reload)
                            (mgl-pax-set-up-doc-buffer doc-dir)))))
          ;; Display the docs of this very documentation browser if
          ;; the input is the empty string.
          (when (string= pax-url "pax:")
            (setq pax-url (mgl-pax-urllike-to-url
                           "pax::@documenting-in-emacs pax:section")))
          ;; No doc buffer. Create a new dir.
          (let ((doc-dir (file-name-as-directory (make-temp-file "pax-doc" t))))
            (mgl-pax-call-document-for-emacs
             pax-url doc-dir
             :ok-cont (lambda (file-url)
                        (let ((package (slime-current-package)))
                          (w3m-goto-url file-url :reload)
                          (setq slime-buffer-package package))
                        (mgl-pax-set-up-doc-buffer doc-dir))
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
  (unless (require 'w3m nil t)
    (error "PAX requires w3m but it cannot be loaded. Please install it."))
  (cl-destructuring-bind (reference fragment)
      (mgl-pax-parse-path-and-fragment schemeless-pax-url)
    (if fragment
        (concat "pax:" (w3m-url-encode-string reference)
                "#" (w3m-url-encode-string fragment))
      (concat "pax:" (w3m-url-encode-string reference)))))

;;; Return the path and fragment part of the schemeless URL.
(defun mgl-pax-parse-path-and-fragment (url)
  (let ((fragment-pos (cl-position ?# url)))
    (if fragment-pos
        (list (cl-subseq url 0 fragment-pos)
              (cl-subseq url (1+ fragment-pos)))
      (list url nil))))

(defun mgl-pax-w3m-goto-url (oldfun url &rest args)
  (if (not (or (string-prefix-p "pax:" url)
               (string-prefix-p "pax-eval:" url)))
      (apply oldfun url args)
    ;; Set up pax e.g. when the user starts w3m, then presses g and
    ;; enters a pax url.
    (unless (mgl-pax-in-doc-buffer-p)
      (mgl-pax-set-up-doc-buffer
       (file-name-as-directory (make-temp-file "pax-doc" t))))
    (let ((buffer (current-buffer)))
      (mgl-pax-call-document-for-emacs url mgl-pax-doc-dir
                                       :ok-cont
                                       (lambda (file-url)
                                         (pop-to-buffer buffer)
                                         (apply oldfun file-url args))))))

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
  (mgl-pax-maybe-autoload
   (lambda (loadedp)
     (if (not loadedp)
         (mgl-pax-not-loaded)
       (mgl-pax-eval-async
        `(cl:funcall (cl:find-symbol (cl:string '#:document-for-emacs)
                                     :mgl-pax)
                     ',url ',dir)
        (lambda (values)
          (if (eq (cl-first values) :file-url)
              (apply ok-cont (cl-rest values))
            (message "%s" (cl-second values))
            (when abort-cont
              (apply abort-cont (cl-rest values)))))
        abort-cont)
       (message "Generating documentation ...")))))

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
                          ',file-url ',dir)
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


;;;; Apropos

(defun mgl-pax-apropos (string &optional external-only package
                               case-sensitive)
  "Show all PAX definitions that match the arguments.
This is a wrapper around MGL-PAX:PAX-APROPOS. STRING is basically
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

With a prefix arg, you're interactively asked for parameters of
the search. Without a prefix arg, EXTERNAL-ONLY defaults to T,
packages and locative types are not filtered, and case does not
matter.

Also, see `mgl-pax-apropos-all'."
  (interactive
   (if current-prefix-arg
       (list (slime-read-from-minibuffer "PAX Apropos: ")
             (y-or-n-p "External symbols only? ")
             (slime-read-package-name "Package: ")
             (y-or-n-p "Case-sensitive? "))
     (list (slime-read-from-minibuffer "PAX Apropos: ") t "" nil)))
  (mgl-pax-document
   (concat "pax-eval:"
           (w3m-url-encode-string
            (prin1-to-string
             `(mgl-pax::pax-apropos* ,string ,external-only
                                     ,package ,case-sensitive))))))

(defun mgl-pax-apropos-all (string)
  "Shortcut for invoking `mgl-pax-apropos` with EXTERNAL-ONLY NIL."
  (interactive (list (slime-read-from-minibuffer "PAX Apropos All: ")))
  (mgl-pax-apropos string nil "" nil))

(defun mgl-pax-apropos-package (package &optional internal)
  "Show apropos listing for symbols in PACKAGE.
With prefix argument include internal symbols."
  (interactive (list (let ((pkg (slime-read-package-name
                                 "PAX Apropos for Package: ")))
                       (if (string= pkg "") (slime-current-package) pkg))
                     current-prefix-arg))
  (mgl-pax-apropos nil (not internal) (concat "'" package) t))


;;;; Transcribe

(defun mgl-pax-transcribe-last-expression ()
  "A bit like C-u C-x C-e (slime-eval-last-expression) that
inserts the output and values of the sexp before the point, this
does the same but with MGL-PAX:TRANSCRIBE. Use a numeric prefix
argument as in index to select one of the Common Lisp
MGL-PAX:*SYNTAXES* as the SYNTAX argument to MGL-PAX:TRANSCRIBE.
Without a prefix argument, the first syntax is used."
  (interactive)
  (mgl-pax-maybe-autoload
   (lambda (loadedp)
     (if (not loadedp)
         (mgl-pax-not-loaded)
       (save-excursion
         (let* ((start (progn (backward-sexp)
                              (move-beginning-of-line nil)
                              (point)))
                (end (progn (forward-sexp)
                            (point))))
           (goto-char end)
           (insert
            (mgl-pax-transcribe start end (mgl-pax-transcribe-syntax-arg)
                                nil nil nil))))))))

(defun mgl-pax-retranscribe-region (start end)
  "Updates the transcription in the current region (as in calling
MGL-PAX:TRANSCRIBE with :UPDATE-ONLY T). Use a numeric prefix
argument as in index to select one of the Common Lisp
MGL-PAX:*TRANSRIBE-SYNTAXES* as the SYNTAX argument to
MGL-PAX:TRANSCRIBE. Without a prefix argument, the syntax of the
input will not be changed."
  (interactive "r")
  (mgl-pax-maybe-autoload
   (lambda (loadedp)
     (if (not loadedp)
         (mgl-pax-not-loaded)
       (let* ((point-at-start-p (= (point) start))
              (point-at-end-p (= (point) end))
              (transcript (mgl-pax-transcribe start end
                                              (mgl-pax-transcribe-syntax-arg)
                                              t t nil)))
         (if point-at-start-p
             (save-excursion
               (goto-char start)
               (delete-region start end)
               (insert transcript))
           (save-excursion
             (goto-char start)
             (delete-region start end))
           (insert transcript)))))))

(defun mgl-pax-transcribe-syntax-arg ()
  (if current-prefix-arg
      (prefix-numeric-value current-prefix-arg)
    nil))

(defun mgl-pax-transcribe (start end syntax update-only echo
                                 first-line-special-p)
  (slime-eval
   `(cl:funcall (cl:find-symbol (cl:string '#:transcribe-for-emacs) :mgl-pax)
                ,(buffer-substring-no-properties start end)
                ',syntax ',update-only ',echo ',first-line-special-p)))

(provide 'mgl-pax)