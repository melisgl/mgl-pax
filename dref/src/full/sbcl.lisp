;;;; SBCL source location support
;;;;
;;;; SB-INTROSPECT:FIND-DEFINITION-SOURCES-BY-NAME returns
;;;; SB-INTROSPECT:DEFINITION-SOURCES objects. We call
;;;; SWANK/SBCL::DEFINITION-SOURCE-FOR-EMACS to convert them into a
;;;; DRef @SOURCE-LOCATIONS, which is DRef's only substantial
;;;; dependency on Swank on SBCL. There is no way around this short of
;;;; duplicating several hundered lines of Swank, which includes
;;;; source-file-cache.lisp, source-path-parser.lisp.
;;;;
;;;; On the other hand, on SBCL we avoid the fragile
;;;; DSPEC-TO-DEFINITION conversion in swank-util.lisp.

(in-package :dref)

;;; Like SB-INTROSPECT:FIND-DEFINITION-SOURCES-BY-NAME, but:
;;;
;;; - If DESCRIPTION is specified, then find the definition source in
;;;   the returned list whose description is equal to it. This is
;;;   useful when there are e.g. multiple :METHODs for NAME.
;;;
;;; - If DESCRIPTION is not specified, then find the single matching
;;;   definition source.
;;;
;;; - Convert the definition source to a source location
;;;   (see @SOURCE-LOCATIONS).
(defun sb-one-source-location (name type &key (description nil descriptionp))
  (let ((defsrcs (sb-introspect:find-definition-sources-by-name name type)))
    (when defsrcs
      (cond (descriptionp
             (loop for defsrc in defsrcs
                   when (equal (sb-introspect::definition-source-description
                                defsrc)
                               description)
                     return (definition-source-to-source-location
                             defsrc type name)))
            (t
             (assert (= (length defsrcs) 1))
             (definition-source-to-source-location (first defsrcs)
                                                   type name))))))

(defvar *do-not-use-swank* nil)

(defun use-swank-p ()
  (and (find-package '#:swank)
       (not *do-not-use-swank*)))

(defun definition-source-to-source-location (defsrc &optional type name)
  (if (use-swank-p)
      (ignore-errors
       (uiop:symbol-call '#:swank/sbcl '#:definition-source-for-emacs
                         defsrc type name))
      (let ((file (sb-introspect:definition-source-pathname defsrc)))
        (when file
          (make-source-location
           :file file
           :file-position (or (ignore-errors (defsrc-file-position defsrc))
                              0))))))

(defun defsrc-file-position (defsrc)
  (let ((pathname (sb-introspect:definition-source-pathname defsrc))
        (offset (sb-introspect:definition-source-character-offset defsrc))
        (form-path (first (sb-introspect:definition-source-form-path defsrc))))
    (when (and pathname (probe-file pathname))
      (with-open-file (s pathname)
        (if offset
            (file-position s offset)
            (let ((*read-suppress* t))
              (loop repeat form-path
                    do (read s nil nil))))
        ;; We are just after the previous form.
        (skip-to-next-form s)
        (file-position s)))))

(defun skip-to-next-form (stream)
  (loop
    (let ((char (peek-char t stream nil nil)))
      ;; FIXME: We should also skip over inactive reader conditionals.
      (if (eql char #\;)
          (read-line stream)
          (return)))))

(defun make-dspec (type name defsrc)
  (if (use-swank-p)
      (ignore-errors
       (uiop:symbol-call '#:swank/sbcl '#:make-dspec type name defsrc))
      (let ((description (sb-introspect::definition-source-description defsrc)))
        (list* type name (ensure-list description)))))


(defparameter *unknown-definition-types*
  '(:transform :deftransform
    :optimizer :defoptimizer
    :vop :define-vop
    :source-transform :define-source-transform
    :ir1-convert :def-ir1-translator
    :declaration declaim
    :alien-type :define-alien-type))

;;; This may return dspecs that correspond to known definition types
;;; if a user defined a locative type for any of
;;; *UNKNOWN-DEFINITION-TYPES*.
(defun at-least-unknown-dspecs (name &key include-location)
  (multiple-value-bind (name foundp) (sb-introspect-definition-name name)
    (when foundp
      (loop
        for type in *unknown-definition-types* by #'cddr
        for defsrcs = (sb-introspect:find-definition-sources-by-name name type)
        append (loop
                 for defsrc in defsrcs
                 collect
                 (let ((dspec (make-dspec type name defsrc)))
                   (if include-location
                       (list dspec
                             (definition-source-to-source-location
                              defsrc type name))
                       dspec)))))))

;;; Turn OBJECT into a symbol suitable as an argument to
;;; SB-INTROSPECT:FIND-DEFINITION-SOURCES-BY-NAME.
(defun sb-introspect-definition-name (object)
  (cond ((stringp object)
         (values (make-symbol object) t))
        ((or (symbolp object)
             (and (listp object)
                  (extended-function-name-p object)))
         (values object t))))

(defun unknown-definitions (name)
  (when (and (or (symbolp name) (stringp name) (listp name)))
    (loop for dspec in (at-least-unknown-dspecs name)
          collect (make-instance 'unknown-dref :name name
                                 :locative `(unknown ,dspec)))))


(defun/autoloaded translate-sb-source-location (sb-source-location)
  (definition-source-to-source-location
   (sb-introspect::translate-source-location sb-source-location)
   nil nil))
