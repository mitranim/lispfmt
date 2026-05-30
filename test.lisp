(defpackage #:lispfmt-tests (:use #:cl))
(in-package #:lispfmt-tests)

(load (merge-pathnames "lispfmt.lisp" *load-truename*))

(defvar *failures* 0)

(defun fail (name expected actual)
  (incf *failures*)
  (format t "~&FAIL: ~A~%Expected:~%~S~%Actual:~%~S~2%" name expected actual))

(defun check= (name expected actual)
  (unless (string= expected actual)
    (fail name expected actual)))

(defun check-error (name thunk)
  (handler-case
      (progn
        (funcall thunk)
        (incf *failures*)
        (format t "~&FAIL: ~A~%Expected an error.~2%" name))
    (lispfmt:formatter-error (condition)
      (declare (ignore condition))
      t)))

(defun fmt (string)
  (lispfmt:format-string string))

(defun s (control &rest args)
  (apply #'format nil control args))

(defun run-tests ()
  (setf *failures* 0)
  (check=
   "structured editing example"
   (s "(defun some-func ()~%  (let~%    (~%      (one 10)~%      (two 20)~%    )~%    (print (+ one two))~%    (print (+ two one))~%  )~%)~%~%")
   (fmt (s "(defun some-func ()~%  (let ((one 10) (two 20))~%    (print (+ one two))~%    (print (+ two one))))")))
  (check=
   "definition macro shape"
   (s "(defmacro with-thing (x) body)~%~%")
   (fmt (s "(defmacro with-thing (x)~%body)")))
  (check=
   "definition variable shape"
   (s "(defvar *x* 10)~%~%")
   (fmt (s "(defvar *x*~%10)")))
  (check=
   "definition parameter shape"
   (s "(defparameter *x* 10)~%~%")
   (fmt (s "(defparameter *x*~%10)")))
  (check=
   "definition class shape"
   (s "(defclass thing ()~%  ((slot :initarg :slot))~%)~%~%")
   (fmt (s "(defclass thing ()~%((slot :initarg :slot)))")))
  (check=
   "definition method qualifier conservative shape"
   (s "(defmethod render~%  :around~%  ((x thing))~%  body~%)~%~%")
   (fmt (s "(defmethod render~%:around~%((x thing))~%body)")))
  (check=
   "definition condition shape"
   (s "(define-condition bad-thing (error)~%  ((reason :initarg :reason))~%)~%~%")
   (fmt (s "(define-condition bad-thing (error)~%((reason :initarg :reason)))")))
  (check=
   "definition struct shape"
   (s "(defstruct point~%  x~%  y~%)~%~%")
   (fmt (s "(defstruct point~%x~%y)")))
  (check=
   "scheme define function shape"
   (s "(define (square x) (* x x))~%~%")
   (fmt (s "(define (square x)~%(* x x))")))
  (check=
   "scheme define variable shape"
   (s "(define pi 3.14)~%~%")
   (fmt (s "(define pi~%3.14)")))
  (check=
   "scheme define inline-safe body"
   (s "(define my-var (+ one two))~%~%")
   (fmt (s "(define~%  my-var~%  (+ one two)~%)")))
  (check=
   "scheme define preserves multiline body"
   (s "(define my-var~%  (+~%    one~%    two~%  )~%)~%~%")
   (fmt (s "(define~%  my-var~%  (+ one~%    two~%  )~%)")))
  (check=
   "scheme define-syntax shape"
   (s "(define-syntax when transformer)~%~%")
   (fmt (s "(define-syntax when~%transformer)")))
  (check=
   "scheme define-record-type shape"
   (s "(define-record-type point~%  constructor~%  predicate~%)~%~%")
   (fmt (s "(define-record-type point~%constructor~%predicate)")))
  (check=
   "definition keyword false positive"
   (s "(def~%  :keyword~%  value~%)~%~%")
   (fmt (s "(def~%:keyword~%value)")))
  (check=
   "definition numeric false positive"
   (s "(deft~%  123~%  body~%)~%~%")
   (fmt (s "(deft~%123~%body)")))
  (check=
   "definition prefix false positive"
   (s "(notdef~%  name~%  args~%  body~%)~%~%")
   (fmt (s "(notdef name~%args~%body)")))
  (check= "prefix hash chain" (s "###one two~%~%") (fmt "# # # one two"))
  (check= "prefix quote list" (s "#'(one two)~%~%") (fmt "# ' ( one two )"))
  (check= "backquote atom number" (s "`123~%~%") (fmt "`123"))
  (check= "backquote atom symbol" (s "`one~%~%") (fmt "`one"))
  (check= "backquote string" (s "`\"str\"~%~%") (fmt "` \"str\""))
  (check= "backquote list" (s "`(one two)~%~%") (fmt "`(one two)"))
  (check= "backquote newline list" (s "`(one two)~%~%") (fmt (s "`~%(one two)")))
  (check= "hash backquote list" (s "#`(one two)~%~%") (fmt "# ` (one two)"))
  (check= "datum comment prefix atom" (s "#;one~%~%") (fmt "# ; one"))
  (check= "datum comment prefix list" (s "#;(one two)~%~%") (fmt "# ; ( one two )"))
  (check= "feature prefix plus atom" (s "#+one~%~%") (fmt "# + one"))
  (check= "feature prefix minus atom" (s "#-one~%~%") (fmt "# - one"))
  (check= "feature prefix plus list" (s "#+(one two)~%~%") (fmt "# + (one two)"))
  (check= "fuzzy character literal single char" (s "#\\A~%~%") (fmt "# \\ A"))
  (check= "fuzzy character literal named char" (s "#\\space~%~%") (fmt "# \\ space"))
  (check=
   "prefix separated from block comment"
   (s "#':~%#|~%one~%|#~%~%")
   (fmt "# ' : #|one|#"))
  (check=
   "nested block comment"
   (s "#|~%one~%#|~%two~%|#~%three~%|#~%~%")
   (fmt "#|one#|two|#three|#"))
  (check=
   "block comment preserves inner indentation"
   (s "#|~%  one~%    two~%|#~%~%")
   (fmt (s "#|  one~%    two|#")))
  (check= "cons dot single line" (s "(one . two)~%~%") (fmt "(one . two)"))
  (check=
   "cons dot multiline"
   (s "(one~%  .~%  two~%)~%~%")
   (fmt (s "(one .~%two)")))
  (check=
   "line comment trailing"
   (s "(one ; two~%)~%~%")
   (fmt (s "(~%one ; two~%)")))
  (check=
   "line comment standalone"
   (s "(~%  one~%  ; two~%)~%~%")
   (fmt (s "(~%one~%; two~%)")))
  (check=
   "vector multiline"
   (s "#(~%  one~%  two~%  three~%)~%~%")
   (fmt (s "#(one two~%three~%)")))
  (check=
   "quoted list multiline"
   (s "'(one~%  two~%  three~%)~%~%")
   (fmt (s "'(one two~%three~%)")))
  (check=
   "full opening token alignment"
   (s "#'#':#(~%  one~%  two~%)~%~%")
   (fmt (s "#'#':#(one two~%)")))
  (check=
   "multiline string closing quote column zero"
   (s "(print \"one~%two~%\"~%)~%~%")
   (fmt (s "(print \"one~%two~%  \"~%)")))
  (check= "blank lines trimmed" (s "one~%~%two~%~%") (fmt (s "one~%~%~%~%two")))
  (check-error "mismatched delimiter" (lambda () (fmt "(]")))
  (check-error "unexpected close" (lambda () (fmt ")")))
  (check-error "whitespace after character literal" (lambda () (fmt "#\\ ")))
  (check-error "fuzzy character literal missing char" (lambda () (fmt "# \\")))
  (let ((samples (list "(one . two)"
                       (s "(~%one~%; two~%)")
                       "#|one#|two|#three|#"
                       "# + one"
                       "# - one"
                       "` \"str\""
                       (s "`~%(one two)")
                       "# ` (one two)"
                       "# \\ space"
                       (s "#|  one~%    two|#")
                       (s "(defmacro with-thing (x)~%body)")
                       (s "(define~%  my-var~%  (+ one two)~%)")
                       (s "(define~%  my-var~%  (+ one~%    two~%  )~%)")
                       (s "(define (square x)~%(* x x))")
                       (s "(defmethod render~%:around~%((x thing))~%body)")
                       (s "#'#':#(one two~%)"))))
    (dolist (sample samples)
      (let ((once (fmt sample)))
        (check= (format nil "idempotent ~A" sample) once (fmt once)))))
  (format t "~&~D failure(s).~%" *failures*)
  (when (plusp *failures*)
    (sb-ext:exit :code 1)))

(run-tests)
