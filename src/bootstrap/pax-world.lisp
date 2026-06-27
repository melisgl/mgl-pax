(in-package :mgl-pax)

(defsection @pax-world (:title "PAX World")
  "PAX World is a registry of documents, which can generate
  cross-linked HTML documentation pages for all the registered
  documents. There is an official PAX World at <https://fixnum.com/>,
  which is updated every few hours. To have your stuff there,
  REGISTER-DOC-IN-PAX-WORLD in your ASDF system and file an issue at
  <https://github.com/melisgl/mgl-pax-world/issues> to make PAX World
  actually load it."
  (register-doc-in-pax-world function)
  "For example, this is how PAX registers itself:"
  (register-doc-example (include (:start (pax-sections function)
                                  :end (end-of-register-doc-example variable))
                                 :header-nl "```"
                                 :footer-nl "```"))
  (update-pax-world function))

;;; (ID SECTIONS-OR-FUNCTION-DESIGNATOR PAGE-SPECS-OR-FUNCTION-DESIGNATOR)
(defvar *registered-pax-world-docs* ())

(defun list-designator (x)
  (if (or (listp x) (symbolp x) (functionp x))
      x
      (list x)))

(defun register-doc-in-pax-world (id sections page-specs)
  "Register SECTIONS and PAGE-SPECS under ID (an arbitrary symbol) in
  PAX World. By default, UPDATE-PAX-WORLD generates documentation for
  all of these. SECTIONS and PAGE-SPECS must be lists of SECTIONs and
  PAGE-SPECs (see @PAGES) or designators of functions with no
  arguments that return such lists.

  Using an existing ID overwrites the previous registration."
  (declare (type symbol id))
  (setq *registered-pax-world-docs*
        (remove id *registered-pax-world-docs* :key #'first))
  (push (list id (list-designator sections) (list-designator page-specs))
        *registered-pax-world-docs*))

(declaim (ftype function make-git-source-uri-fn))
(declaim (ftype function make-github-source-uri-fn))
