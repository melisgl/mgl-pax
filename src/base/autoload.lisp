(in-package :mgl-pax)

;;;; A bit of foreshadowing of MGL-PAX/FULL.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *transcribe-check-consistency*)
  (export '*transcribe-check-consistency*)
  (defvar *transcribe-syntaxes*)
  (export '*transcribe-syntaxes*)
  (defvar *document-uppercase-is-code*)
  (export '*document-uppercase-is-code*)
  (defvar *document-link-code*)
  (export '*document-link-code*)
  (defvar *document-link-sections*)
  (export '*document-link-sections*)
  (defvar *document-max-numbering-level*)
  (export '*document-max-numbering-level*)
  (defvar *document-max-table-of-contents-level*)
  (export '*document-max-table-of-contents-level*)
  (defvar *document-text-navigation*)
  (export '*document-text-navigation*)
  (defvar *document-fancy-html-navigation*)
  (export '*document-fancy-html-navigation*)
  (defvar *document-mark-up-signatures*)
  (export '*document-mark-up-signatures*)
  (defvar *document-normalize-packages*)
  (export '*document-normalize-packages*)
  (defvar *document-html-top-blocks-of-links*)
  (export '*document-html-top-blocks-of-links*)
  (defvar *document-html-bottom-blocks-of-links*)
  (export '*document-html-bottom-blocks-of-links*)
  (defvar *document-html-max-navigation-table-of-contents-level*)
  (export '*document-html-max-navigation-table-of-contents-level*)
  (defvar *format*)
  (export '*format*))

;;;; FIXME: The following should be turned into a doc extension API.

;;; FIXME: Need the macros instead.
(declaim (special *local-references*))
(declaim (special *reference-being-documented*))

(declaim (ftype function locate-and-print-bullet))
(declaim (ftype function print-arglist))
(declaim (ftype function print-bullet))
(declaim (ftype function print-end-bullet))
(declaim (ftype function massage-docstring))
(declaim (ftype function maybe-print-docstring))
(declaim (ftype function documentation*))
(declaim (ftype function escape-markdown))
(declaim (ftype function prin1-and-escape-markdown))


;;; These are used only by the DOCUMENT-OBJECT for CLASSes.
(declaim (ftype function global-reference-p))
(declaim (ftype function link-to-reference))

;;; And these by DOCUMENT-OBJECT for SECTIONs.
(defmacro with-heading ((stream object title &key link-title-to)
                        &body body)
  `(call-with-heading ,stream ,object ,title ,link-title-to
                      (lambda (,stream) ,@body)))
(declaim (ftype function call-with-heading))

(defmacro with-nested-headings (() &body body)
  `(let ((*heading-level* (1+ *heading-level*)))
     ,@body))
(declaim (special *heading-level*))


(defmacro defun/autoload (name asdf-system-name &key (export t))
  `(unless (fboundp ',name)
     (declaim (notinline ,name))
     (defun ,name (&rest args)
       ;; Prevent infinite recursion which would happen if the loaded
       ;; system doesn't redefine the function.
       (setf (symbol-function ',name)
             (lambda (&rest args)
               (declare (ignore args))
               (error "Autoloading ~S failed." ',name)))
       ;; FIXME: Silence redefinition warnings for autoload functions
       ;; somehow?
       (asdf:load-system ,asdf-system-name)
       ;; Make sure that the function redefined by LOAD-SYSTEM is
       ;; invoked and not this stub, which could be the case without
       ;; the SYMBOL-FUNCTION call.
       (apply (symbol-function ',name) args))
     ,@(when export
         `((export ',name)))))

(defun/autoload locate-definitions-for-emacs '#:mgl-pax/navigate :export nil)
(defun/autoload find-hyperspec-id '#:mgl-pax/document :export nil)
(defun/autoload downcasingp '#:mgl-pax/document :export nil)
(defun/autoload document '#:mgl-pax/document)
(defun/autoload update-asdf-system-readmes '#:mgl-pax/document)
(defun/autoload update-asdf-system-html-docs '#:mgl-pax/document)
;;; UPDATE-PAX-WORLD includes PAX itself, so load MGL-PAX/FULL to have
;;; all documentation. Otherwise, MGL-PAX/DOCUMENT would be enough.
(defun/autoload update-pax-world '#:mgl-pax/full)
(defun/autoload transcribe '#:mgl-pax/transcribe)
(defun/autoload transcribe-for-emacs '#:mgl-pax/transcribe)
(defun/autoload squeeze-whitespace '#:mgl-pax/transcribe)
(defun/autoload delete-trailing-whitespace '#:mgl-pax/transcribe)
(defun/autoload delete-comments '#:mgl-pax/transcribe)
