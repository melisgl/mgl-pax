(in-package :mgl-pax)

;;;; A bit of foreshadowing of MGL-PAX/FULL.

;;;; FIXME: The following should be turned into an doc api.

(declaim (special *references*))
(declaim (special *local-references*))
(declaim (special *document-mark-up-signatures*))
(declaim (special *format*))
(declaim (special *document-normalize-packages*))
(declaim (special *reference-being-documented*))
(declaim (ftype function link-to-reference))
(declaim (ftype function codify-and-autolink))
(declaim (ftype function locate-and-print-bullet))
(declaim (ftype function print-arglist))
(declaim (ftype function print-bullet))
(declaim (ftype function print-end-bullet))
(declaim (ftype function maybe-downcase))
(declaim (ftype function massage-docstring))
(declaim (ftype function maybe-print-docstring))
(declaim (ftype function filter-documentation))
(declaim (ftype function escape-markdown))
(declaim (ftype function prin1-and-escape-markdown))

(defmacro with-heading ((stream object title &key link-title-to)
                        &body body)
  `(call-with-heading ,stream ,object ,title ,link-title-to
                      (lambda (,stream) ,@body)))
(declaim (ftype function call-with-heading))

(defmacro with-nested-headings (() &body body)
  `(let ((*heading-level* (1+ *heading-level*)))
     ,@body))
(declaim (special *heading-level*))


(defmacro define-autoload-function (name asdf-system-name)
  `(unless (fboundp ',name)
     (declaim (notinline ,name))
     (defun ,name (&rest args)
       ;; Prevent infinite recursion which would happen the loaded
       ;; system doesn't redefine the function.
       (setf (symbol-function ',name)
             (lambda (&rest args)
               (declare (ignore args))
               (error "Autoloading ~S failed." ',name)))
       ;; FIXME: Silence redefinition warnings for autoload
       ;; functions only somehow?
       (asdf:load-system ,asdf-system-name)
       ;; Make sure that the function redefined by LOAD-SYSTEM is
       ;; invoked and not this stub, which could be the case without
       ;; the SYMBOL-FUNCTION call.
       (apply (symbol-function ',name) args))
     (export ',name)))

(define-autoload-function locate-definitions-for-emacs :mgl-pax/navigate)
(define-autoload-function document :mgl-pax/document)
(define-autoload-function update-asdf-system-readmes :mgl-pax/document)
(define-autoload-function update-asdf-system-html-docs :mgl-pax/document)
;;; UPDATE-PAX-WORLD includes PAX itself, so load MGL-PAX/FULL to have
;;; all documentation. Otherwise, MGL-PAX/DOCUMENT would be enough.
(define-autoload-function update-pax-world :mgl-pax/full)
(define-autoload-function transcribe :mgl-pax/transcribe)
(define-autoload-function transcribe-for-emacs :mgl-pax/transcribe)
