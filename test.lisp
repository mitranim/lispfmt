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
  (check= "keyword pairs single line unchanged"
          (s "(dict :one 10 :two 20 :three 30)~%~%")
          (fmt "(dict :one 10 :two 20 :three 30)"))
  (check=
   "keyword pairs multiline"
   (s "(dict~%  :one 10~%  :two 20~%  :three 30~%)~%~%")
   (fmt (s "(dict~%  :one 10 :two 20 :three 30~%)")))
  (check= "keyword-only pairs single line unchanged"
          (s "(dict :one :two :three :four)~%~%")
          (fmt "(dict :one :two :three :four)"))
  (check=
   "keyword-only pairs multiline"
   (s "(dict~%  :one :two~%  :three :four~%)~%~%")
   (fmt (s "(dict~%  :one :two :three :four~%)")))
  (check=
   "keyword missing value"
   (s "(dict~%  :one 10~%  :two~%)~%~%")
   (fmt (s "(dict~%  :one 10 :two~%)")))
  (check=
   "keyword multiline value guard"
   (s "(dict~%  :one~%  (value~%    one~%    two~%  )~%)~%~%")
   (fmt (s "(dict~%  :one (value one~%    two~%  )~%)")))
  (check=
   "loop keyword clauses"
   (s "(loop~%  :from 0~%  :for previous = nil~%  :then form~%  :do ...~%)~%~%")
   (fmt (s "(loop~%  :from 0 :for previous = nil :then form :do ...~%)")))
  (check=
   "loop bare keywords normalized"
   (s "(loop~%  :for form~%  :in forms~%  :for gap~%  :in gaps~%  :for index~%  :from 0~%  :for previous = nil~%  :then form~%  :do~%  ; elided~%)~%~%")
   (fmt (s "(loop for form in forms~%      for gap in gaps~%      for index from 0~%      for previous = nil then form~%      do~%      ; elided~%)")))
  (check=
   "loop package keyword normalized"
   (s "(loop~%  :for x~%  :in xs~%)~%~%")
   (fmt (s "(loop cl-user::for x cl-user::in xs~%)")))
  (check=
   "loop uppercase keyword normalized"
   (s "(loop~%  :FOR x~%  :IN xs~%)~%~%")
   (fmt (s "(loop FOR x IN xs~%)")))
  (check=
   "loop with and collect into normalized"
   (s "(loop~%  :with x = 1~%  :and y = 2~%  :collect x~%  :into xs~%)~%~%")
   (fmt (s "(loop with x = 1 and y = 2 collect x into xs~%)")))
  (check=
   "loop when else end normalized"
   (s "(loop~%  :when ready~%  :collect x~%  :else~%  :collect y~%  :end~%)~%~%")
   (fmt (s "(loop when ready collect x else collect y end~%)")))
  (check=
   "loop hash clauses normalized"
   (s "(loop~%  :for k~%  :being~%  :the~%  :hash-keys~%  :of table~%  :using value~%)~%~%")
   (fmt (s "(loop for k being the hash-keys of table using value~%)")))
  (check=
   "loop initially finally normalized"
   (s "(loop~%  :initially (setup)~%  :finally (finish)~%)~%~%")
   (fmt (s "(loop initially (setup) finally (finish)~%)")))
  (check=
   "loop nested payload not normalized"
   (s "(loop~%  :for x~%  :in (list for in collect)~%  :collect (make for)~%)~%~%")
   (fmt (s "(loop for x in (list for in collect) collect (make for)~%)")))
  (check=
   "loop it payload not normalized"
   (s "(loop~%  :for it~%  :in xs~%  :collect it~%)~%~%")
   (fmt (s "(loop~%  for it in xs collect it~%)")))
  (check=
   "loop named payload not normalized"
   (s "(loop~%  :for named~%  :in xs~%  :collect named~%)~%~%")
   (fmt (s "(loop~%  for named in xs collect named~%)")))
  (check=
   "loop for payload not normalized"
   (s "(loop~%  :for for~%  :in xs~%  :collect for~%)~%~%")
   (fmt (s "(loop~%  for for in xs collect for~%)")))
  (check=
   "loop typed with assignment"
   (s "(loop~%  :with x fixnum = 1~%  :collect x~%)~%~%")
   (fmt (s "(loop~%  with x fixnum = 1 collect x~%)")))
  (check=
   "loop with assignment"
   (s "(loop~%  :with x = 1~%  :collect x~%)~%~%")
   (fmt (s "(loop~%  with x = 1 collect x~%)")))
  (check=
   "loop typed for from"
   (s "(loop~%  :for i fixnum~%  :from 0~%  :collect i~%)~%~%")
   (fmt (s "(loop~%  for i fixnum from 0 collect i~%)")))
  (check=
   "loop of-type for"
   (s "(loop~%  :for x :of-type fixnum~%  :in xs~%  :collect x~%)~%~%")
   (fmt (s "(loop~%  for x of-type fixnum in xs collect x~%)")))
  (check=
   "loop typed collision payload"
   (s "(loop~%  :for it fixnum~%  :in xs~%  :collect it~%)~%~%")
   (fmt (s "(loop~%  for it fixnum in xs collect it~%)")))
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
                       (s "(dict~%  :one 10 :two 20 :three 30~%)")
                       (s "(dict~%  :one :two :three :four~%)")
                       (s "(loop~%  :from 0 :for previous = nil :then form :do ...~%)")
                       (s "(loop for form in forms~%      do form)")
                       (s "(loop cl-user::for x cl-user::in xs~%)")
                       (s "(loop~%  for it in xs collect it~%)")
                       (s "(loop~%  for i fixnum from 0 collect i~%)")
                       (s "#'#':#(one two~%)"))))
    (dolist (sample samples)
      (let ((once (fmt sample)))
        (check= (format nil "idempotent ~A" sample) once (fmt once)))))
  (format t "~&~D failure(s).~%" *failures*)
  (when (plusp *failures*)
    (sb-ext:exit :code 1)))

(run-tests)
