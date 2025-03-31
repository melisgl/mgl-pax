(cl:in-package :dref)

(in-readtable pythonic-string-syntax)

(defun dtypexpand (dtype-specifier)
  ;; NIL expands to NIL
  (when dtype-specifier
    (destructuring-bind (name &rest args) (ensure-list dtype-specifier)
      (let ((expander (gethash name *dtype-expanders*)))
        (if expander
            (values (apply expander args) t)
            (values dtype-specifier nil))))))

(define-dtype top ()
  "This is the top of the DTYPE hierarchy, much like T for Lisp types.
  It expands to ([OR][type] T PSEUDO). While T matches every normal
  Lisp object and objectless definitions present in the running
  Lisp (see LISP-LOCATIVE-TYPES), TOP matches even pseudo
  definitions (see PSEUDO-LOCATIVE-TYPES)."
  '(or t pseudo))

(define-dtype pseudo ()
  "This is the union of all PSEUDO-LOCATIVE-TYPES. It expands
  to `(OR ,@(PSEUDO-LOCATIVE-TYPES))`."
  `(or ,@(pseudo-locative-types)))


(defun/autoloaded dtypep (object dtype)
  """Like CL:TYPEP, but OBJECT may be an XREF and DTYPE may involve
  @LOCATIVE-TYPEs and full @LOCATIVEs.

  - OBJECT may be an XREF (including DREFs). `(DTYPEP OBJECT DTYPE)`
    is equivalent to `(DTYPEP (LOCATE OBJECT) DTYPE)`. This form is
    necesary when the definition has no first-class object associated
    with it:

      ```cl-transcript (:dynenv dref-std-env)
      (dtypep (dref 'pi 'constant) 'variable)
      => T
      ```

      Note however that as a consequence, DTYPEP deviates from TYPEP
      when OBJECT is an XREF:

      ```cl-transcript (:dynenv dref-std-env)
      (typep (dref 'number 'class) 'class)
      => NIL
      (dtypep (dref 'number 'class) 'class)
      => T
      (typep (xref 'number 'junk) 'standard-object)
      => T
      (dtypep (xref 'number 'junk) 'standard-object)
      => NIL
      (dtypep (xref 'number 'junk) 'junk)
      => NIL
      ```

      [MEMBER][type] and [SATISFIES][type] are only matched with the
      RESOLVEd OBJECT if any:

      ```cl-transcript (:dynenv dref-std-env)
      (let ((dref (dref nil 'type)))
        (typep dref `(member ,dref)))
      => T
      (let ((dref (dref nil 'type)))
        (dtypep dref `(member ,dref)))
      => NIL
      ```

  - If OBJECT is not of type XREF (including DREFs) and DTYPE is a
    valid Lisp [type specifier][clhs], then DTYPEP is equivalent to
    TYPEP:

      ```cl-transcript (:dynenv dref-std-env)
      (defparameter *c* (find-class 'number))
      (defparameter *m* (find-method #'dref* () '(t t t)))
      (typep *c* 'class)
      => T
      (dtypep *c* 'class)
      => T
      (typep *m* 'method)
      => T
      (dtypep *m* 'method)
      => T
      (typep 4 '(integer 3 5))
      => T
      (dtypep 4 '(integer 3 5))
      => T
      ```

  - DTYPE may be a @LOCATIVE-TYPE:

      ```cl-transcript (:dynenv dref-std-env)
      (dtypep (dref nil 'type) 'type)
      => T
      ```

  - DTYPE may be a full @LOCATIVE:

      ```cl-transcript (:dynenv dref-std-env)
      (typep *c* '(class))
      .. debugger invoked on SIMPLE-ERROR:
      ..   unknown type specifier: (CLASS)
      (dtypep *c* '(class))
      => T
      (dtypep *c* '(class junk))
      .. debugger invoked on SIMPLE-ERROR:
      ..   Bad arguments (JUNK) for locative CLASS with lambda list NIL.
      (typep *m* '(method () (t t t)))
      .. debugger invoked on SIMPLE-ERROR:
      ..   unknown type specifier: (METHOD NIL (T T T))
      (dtypep *m* '(method () (t t t)))
      => T
      ```

      If a full locative matches, then its locative type (without the
      locative args) also does.

  - DTYPE may be named by DEFINE-DTYPE:

      ```cl-transcript (:dynenv dref-std-env)
      (dtypep 7 'top)
      => T
      ```

  - DTYPE may be constructed with [AND][type], [OR][type] and
    [NOT][type] from Lisp types, locative types, full locatives and
    named DTYPEs:

      ```cl-transcript (:dynenv dref-std-env)
      (typep 7 '(or type string))
      .. debugger invoked on SIMPLE-ERROR:
      ..   unknown type specifier: TYPE
      (dtypep 7 '(or type string))
      => NIL
      (dtypep 7 '(or integer type))
      => T
      (dtypep *c* '(or condition class))
      => T
      (dtypep (dref 'locate-error 'condition) '(or condition class))
      => T
      (dtypep (dref nil 'type) '(and type (not class)))
      => T
      ```"""
  (multiple-value-bind (resolved unresolvedp) (ignore-errors (resolve object))
    (let* ((resolvedp (not unresolvedp))
           (dref (locate object nil))
           (object-name (and dref (dref-name dref)))
           (object-locative-type (and dref (dref-locative-type dref)))
           (pseudop (member object-locative-type *pseudo-locative-types*)))
      (when (or dref resolvedp)
        (labels
            ((object-is-of-locative-type-p (locative-type)
               (or (and dref (locative-subtype-p object-locative-type
                                                 locative-type))
                   (and resolvedp
                        (typep resolved (widest-subtype-of-locative-type
                                         locative-type)))))
             (recurse (dtype)
               ;; CCL is terribly slow with some invalid types
               ;; https://github.com/Clozure/ccl/issues/531
               (unless (progn
                         #+ccl (eq (type-specifier-name dtype) 'not)
                         #-ccl nil)
                 (cond
                   ((not resolvedp)
                    ;; LOCATIVE-ARGS can only restrict LOCATIVE-TYPE.
                    (when (and (not pseudop)
                               (subtypep* (narrowest-supertype-of-locative-type
                                           object-locative-type)
                                          dtype))
                      (return-from recurse t)))
                   ((member (type-specifier-name dtype) '(member satisfies))
                    ;; Let the errors through.
                    (return-from recurse (typep resolved dtype)))
                   (t
                    (multiple-value-bind (matchedp valid-lisp-type-p)
                        (typep* resolved dtype)
                      (when valid-lisp-type-p
                        (return-from recurse matchedp))))))
               (setq dtype (dtypexpand dtype))
               (cond
                 ;; E.g. FUNCTION or METHOD
                 ((atom dtype)
                  (object-is-of-locative-type-p dtype))
                 ((eq (first dtype) 'and)
                  (loop for child in (rest dtype) always (recurse child)))
                 ((eq (first dtype) 'or)
                  (loop for child in (rest dtype) thereis (recurse child)))
                 ((eq (first dtype) 'not)
                  (unless (= (length dtype) 2)
                    (error "Invalid type specifier ~S." dtype))
                  (not (recurse (second dtype))))
                 ((member-type-specifier-p dtype)
                  nil)
                 ((satisfies-type-specifier-p dtype)
                  (unless (valid-satisisfies-type-specifier-args-p (rest dtype))
                    (error "~@<~S is not a valid ~S type.~:@>"
                           dtype 'satisfies)))
                 ;; E.g. (FUNCTION) or (METHOD NIL (NUMBER))
                 (t
                  (check-locative-type (first dtype))
                  (check-locative-args* (first dtype) (rest dtype))
                  (if (= (length dtype) 1)
                      (object-is-of-locative-type-p (first dtype))
                      ;; LOCATIVE-ARGS restrict LOCATIVE-TYPE in
                      ;; unknown ways, so we must match
                      ;; LOCATIVE-TYPE exactly.
                      (when dref
                        (or (equal dtype (dref-locative dref))
                            ;; DREF-TEST::TEST-DTYPEP/WITH-LOCATIVE-ARGS/ACTUALIZED
                            (let ((dref1 (dref object-name dtype nil)))
                              (and dref1 (xref= dref1 dref))))))))))
          (values (recurse dtype)))))))

(defun locative-type-direct-supers (locative-type)
  (remove nil (mapcar #'dref-class-name-to-locative
                      (second (%locative-type-class-info locative-type)))))

(defun locative-type-direct-subs (locative-type)
  (let ((dref-class-name (locative-type-dref-class locative-type)))
    (loop for locative-type being the hash-key in *locative-type-to-class-info*
            using (hash-value info)
          when (member dref-class-name (second info))
            collect locative-type)))

(defun locative-subtypes (locative-type)
  (cond ((eq locative-type nil)
         ())
        ;; T is not a locative type but a DTYPE. This is just for the
        ;; convenience of COVER-BASIC-DTYPE.
        ((eq locative-type t)
         *lisp-locative-types*)
        (t
         (check-locative-type locative-type)
         (loop for locative-type-1 in (locative-types)
               when (locative-subtype-p locative-type-1 locative-type)
                 collect locative-type-1))))

(defun locative-subtype-p (locative-type-1 locative-type-2)
  (cond ((or (eq locative-type-1 locative-type-2)
             (eq locative-type-1 nil)
             (eq locative-type-2 'top))
         (values t t))
        ((eq locative-type-2 t)
         (find locative-type-1 *lisp-locative-types*))
        (t
         (let ((class1 (locative-type-dref-class locative-type-1))
               (class2 (locative-type-dref-class locative-type-2)))
           (if (or (null class1) (null class2))
               (values nil t)
               (subtypep class1 class2))))))

;;; Return the largest Lisp type that's a subtype of of the class
;;; named LOCATIVE-TYPE. If LOCATIVE-TYPE does not name a class, then
;;; NIL is returned.
(defun widest-subtype-of-locative-type (locative-type)
  (let ((roots ()))
    (labels ((recurse (locative-type)
               (unless (member locative-type roots)
                 (if (find-class locative-type nil)
                     (push locative-type roots)
                     (mapc #'recurse
                           (locative-type-direct-subs locative-type))))))
      (recurse locative-type))
    (case (length roots)
      ((0) nil)
      ((1) (first roots))
      (t `(or ,@roots)))))

;;; Return the smallest containing Lisp type for LOCATIVE-TYPE.
(defun narrowest-supertype-of-locative-type (locative-type)
  (let ((roots ()))
    (labels ((recurse (locative-type)
               (unless (member locative-type roots)
                 (if (find-class locative-type nil)
                     (push locative-type roots)
                     (mapc #'recurse
                           (locative-type-direct-supers locative-type))))))
      (recurse locative-type))
    (case (length roots)
      ((0) t)
      ((1) (first roots))
      (t `(and ,@roots)))))

(defun eql-or-xref= (obj1 obj2)
  (or (eql obj1 obj2)
      (let ((dref1 (locate obj1 nil))
            (dref2 (locate obj2 nil)))
        (and dref1 dref2 (xref= dref1 dref2)))))


;;;; For DREF-APROPOS, querying all locative types is expensive and
;;;; often unnecessary (if its DTYPE argument rules certain locative
;;;; types out) since the set of all definitions is at least in the
;;;; tens of thousands. So, we query like this:
;;;;
;;;; 1. COVER-DTYPE gives a set of locative types whose union contains
;;;;    all definitions of DTYPE, but this upper bound may be loose if
;;;;    Lisp types that do not correspond to a single locative type
;;;;    are involved or when a locative args restict a locative type
;;;;    further as in (METHOD () (NUMBER)).
;;;;
;;;; 2. We gather all definitions with these locative types.
;;;;
;;;; 3. FILTER-DREFS-BY-DTYPE discards the definitions that are not of
;;;;    DTYPE (possible if the upper bound is loose).

(defun cover-dtype (dtype)
  (values (cover-dtype* dtype nil)))

(defun cover-dtype* (dtype negatep)
  ;; Expanding gets rid of one level of derived types (but children
  ;; may still be derived). If we don't expand, we only loosen the
  ;; bound because COVER-BASIC-DTYPE, SUPPORT-BASIC-DTYPE and the last
  ;; branch of the COND below are conservative.
  (let ((dtype (dtypexpand (typexpand dtype))))
    (flet ((child-sets (children)
             (loop for child in children
                   collect (cover-dtype* child negatep))))
      (case (and (not (atom dtype)) (first dtype))
        ((and)
         (reduce #'intersection (child-sets (rest dtype))
                 :initial-value *locative-types*))
        ((or)
         (reduce #'union (child-sets (rest dtype))
                 :initial-value ()))
        ((not)
         (unless (= (length dtype) 2)
           (error "Invalid type specifier ~S." dtype))
         (set-difference *locative-types*
                         (cover-dtype* (second dtype) (not negatep))))
        ((member)
         (reduce #'union (mapcar #'cover-object-with-locative-types
                                 (rest dtype))
                 :initial-value ()))
        ((satisfies)
         *locative-types*)
        (t
         (locative-subtypes (if negatep
                                (support-dtype/single dtype)
                                (cover-dtype/single dtype))))))))

(defun cover-object-with-locative-types (object)
  (when-let ((dref (locate object nil)))
    (list (dref-locative-type dref))))

;;; In the base case, we cover DTYPE with a single locative type, NIL
;;; or T (where NIL and T are not locative types) meaning that
;;; COVER-DTYPE-WITH-LOCATIVE-TYPE returns a locative type L such that
;;; (DTYPEP OBJ DTYPE) implies (DTYPEP OBJ L) for all LOCATEable OBJ,
;;; T if there is no such locative type, or NIL if there can be no
;;; definitions of DTYPE.
;;;
;;; - L is minimal in the sense that none of its sublocativetypes
;;;   satisfy the "DTYPE implies L" condition above.
;;;
;;; - The LOCATEable requirement is there because our goal is
;;;   filtering definitions. E.g. we want to return NIL when DTYPE is
;;;   NUMBER because NUMBER objects have no definition. FIXME
(defun cover-dtype/single (dtype)
  (cond
    ;; Handle METHOD, (METHOD), (METHOD () (NUMBER)).
    ((find-locative-type (validated-locative-type dtype)))
    ((eq dtype nil) nil)
    ((eq dtype t) t)
    ;; Lisp types
    ((valid-type-specifier-p dtype)
     (let ((best-locative-type t)
           (best-class t))
       (dolist (locative-type *locative-types*)
         (let ((class (find-class locative-type nil)))
           (when (and class
                      (subtypep dtype class)
                      (subtypep class best-class))
             (setq best-locative-type locative-type
                   best-class class))))
       best-locative-type))
    (t (error "~@<Invalid ~S ~S.~:@>" 'dtype dtype))))

(defun support-dtype/single (dtype)
  (cond ((find-locative-type (validated-locative-type dtype)))
        ((eq dtype nil) nil)
        ((eq dtype t) t)
        ((valid-type-specifier-p dtype)
         (let ((best-locative-type nil)
               (best-class nil))
           (dolist (locative-type *locative-types*)
             (let ((class (find-class locative-type nil)))
               (when (and class
                          (subtypep best-class class)
                          (subtypep class dtype))
                 (setq best-locative-type locative-type
                       best-class class))))
           best-locative-type))
        (t (error "~@<Invalid ~S ~S.~:@>" 'dtype dtype))))

;;; If DTYPE-SPECIFIER is a
;; - an atomic locative type, e.g. METHOD, or
;; - a compond locative, e.g. (METHOD () (NUMBER)).
;;; then return the locative type. If a compound form, check that it
;;; is valid.
(defun validated-locative-type (dtype-specifier)
  (let ((name (type-specifier-name dtype-specifier)))
    (when (find-locative-type name)
      (when (listp dtype-specifier)
        (check-locative-args* name (rest dtype-specifier)))
      name)))
