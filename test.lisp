(defpackage #:lispfmt-tests (:use #:cl))
(in-package #:lispfmt-tests)

(load (merge-pathnames "lispfmt.lisp" *load-truename*))

(defvar *failures* 0)

(defun fail (name expected actual)
  (incf *failures*)
  (format t "~&FAIL: ~A~%Expected:~%~S~%Actual:~%~S~2%" name expected actual)
)

(defun check= (name expected actual)
  (unless
    (string= expected actual)
    (fail name expected actual)
  )
)

(defun check-error (name thunk)
  (handler-case
    (progn
      (funcall thunk)
      (incf *failures*)
      (format t "~&FAIL: ~A~%Expected an error.~2%" name)
    )
    (lispfmt:formatter-error
      (condition)
      (declare (ignore condition))
      t
    )
  )
)

(defun fmt (string) (lispfmt:format-string string))

(defun tmpl (control &rest args) (apply #'format nil control args))

(defun run-tests ()
  (setf *failures* 0)
  (check= "empty input" "" (fmt ""))
  (check= "whitespace input" "" (fmt (tmpl "  ~%~%  ")))
  (check=
    "structured editing example"
    (tmpl "(defun some-func ()~%  (let~%    (~%      (one 10)~%      (two 20)~%    )~%    (print (+ one two))~%    (print (+ two one))~%  )~%)~%")
    (fmt (tmpl "(defun some-func ()~%  (let ((one 10) (two 20))~%    (print (+ one two))~%    (print (+ two one))))"))
  )
  (check=
    "definition macro shape"
    (tmpl "(defmacro with-thing (x) body)~%")
    (fmt (tmpl "(defmacro with-thing (x)~%body)"))
  )
  (check=
    "definition variable shape"
    (tmpl "(defvar *x* 10)~%")
    (fmt (tmpl "(defvar *x*~%10)"))
  )
  (check=
    "definition parameter shape"
    (tmpl "(defparameter *x* 10)~%")
    (fmt (tmpl "(defparameter *x*~%10)"))
  )
  (check=
    "definition class shape"
    (tmpl "(defclass thing ()~%  ((slot :initarg :slot))~%)~%")
    (fmt (tmpl "(defclass thing ()~%((slot :initarg :slot)))"))
  )
  (check=
    "definition method qualifier conservative shape"
    (tmpl "(defmethod render~%  :around~%  ((x thing))~%  body~%)~%")
    (fmt (tmpl "(defmethod render~%:around~%((x thing))~%body)"))
  )
  (check=
    "definition condition shape"
    (tmpl "(define-condition bad-thing (error)~%  ((reason :initarg :reason))~%)~%")
    (fmt (tmpl "(define-condition bad-thing (error)~%((reason :initarg :reason)))"))
  )
  (check=
    "definition struct shape"
    (tmpl "(defstruct point~%  x~%  y~%)~%")
    (fmt (tmpl "(defstruct point~%x~%y)"))
  )
  (check=
    "scheme define function shape"
    (tmpl "(define (square x) (* x x))~%")
    (fmt (tmpl "(define (square x)~%(* x x))"))
  )
  (check=
    "scheme define variable shape"
    (tmpl "(define pi 3.14)~%")
    (fmt (tmpl "(define pi~%3.14)"))
  )
  (check=
    "scheme define inline-safe body"
    (tmpl "(define my-var (+ one two))~%")
    (fmt (tmpl "(define~%  my-var~%  (+ one two)~%)"))
  )
  (check=
    "scheme define preserves multiline body"
    (tmpl "(define my-var~%  (+~%    one~%    two~%  )~%)~%")
    (fmt (tmpl "(define~%  my-var~%  (+ one~%    two~%  )~%)"))
  )
  (check=
    "scheme define-syntax shape"
    (tmpl "(define-syntax when transformer)~%")
    (fmt (tmpl "(define-syntax when~%transformer)"))
  )
  (check=
    "scheme define-record-type shape"
    (tmpl "(define-record-type point~%  constructor~%  predicate~%)~%")
    (fmt (tmpl "(define-record-type point~%constructor~%predicate)"))
  )
  (check=
    "definition keyword false positive"
    (tmpl "(def~%  :keyword~%  value~%)~%")
    (fmt (tmpl "(def~%:keyword~%value)"))
  )
  (check=
    "definition numeric false positive"
    (tmpl "(deft~%  123~%  body~%)~%")
    (fmt (tmpl "(deft~%123~%body)"))
  )
  (check=
    "definition prefix false positive"
    (tmpl "(notdef~%  name~%  args~%  body~%)~%")
    (fmt (tmpl "(notdef name~%args~%body)"))
  )
  (check=
    "keyword pairs single line unchanged"
    (tmpl "(dict :one 10 :two 20 :three 30)~%")
    (fmt "(dict :one 10 :two 20 :three 30)")
  )
  (check=
    "keyword pairs multiline"
    (tmpl "(dict~%  :one 10~%  :two 20~%  :three 30~%)~%")
    (fmt (tmpl "(dict~%  :one 10 :two 20 :three 30~%)"))
  )
  (check=
    "keyword-only pairs single line unchanged"
    (tmpl "(dict :one :two :three :four)~%")
    (fmt "(dict :one :two :three :four)")
  )
  (check=
    "keyword-only pairs multiline"
    (tmpl "(dict~%  :one :two~%  :three :four~%)~%")
    (fmt (tmpl "(dict~%  :one :two :three :four~%)"))
  )
  (check=
    "keyword missing value"
    (tmpl "(dict~%  :one 10~%  :two~%)~%")
    (fmt (tmpl "(dict~%  :one 10 :two~%)"))
  )
  (check=
    "keyword multiline value guard"
    (tmpl "(dict~%  :one~%  (value~%    one~%    two~%  )~%)~%")
    (fmt (tmpl "(dict~%  :one (value one~%    two~%  )~%)"))
  )
  (check=
    "loop keyword clauses"
    (tmpl "(loop~%  :from 0~%  :for previous = nil~%  :then form~%  :do ...~%)~%")
    (fmt (tmpl "(loop~%  :from 0 :for previous = nil :then form :do ...~%)"))
  )
  (check=
    "loop bare keywords normalized"
    (tmpl "(loop~%  :for form~%  :in forms~%  :for gap~%  :in gaps~%  :for index~%  :from 0~%  :for previous = nil~%  :then form~%  :do~%  ; elided~%)~%")
    (fmt (tmpl "(loop for form in forms~%      for gap in gaps~%      for index from 0~%      for previous = nil then form~%      do~%      ; elided~%)"))
  )
  (check=
    "loop package keyword normalized"
    (tmpl "(loop~%  :for x~%  :in xs~%)~%")
    (fmt (tmpl "(loop cl-user::for x cl-user::in xs~%)"))
  )
  (check=
    "loop uppercase keyword normalized"
    (tmpl "(loop~%  :FOR x~%  :IN xs~%)~%")
    (fmt (tmpl "(loop FOR x IN xs~%)"))
  )
  (check=
    "loop with and collect into normalized"
    (tmpl "(loop~%  :with x = 1~%  :and y = 2~%  :collect x~%  :into xs~%)~%")
    (fmt (tmpl "(loop with x = 1 and y = 2 collect x into xs~%)"))
  )
  (check=
    "loop when else end normalized"
    (tmpl "(loop~%  :when ready~%  :collect x~%  :else~%  :collect y~%  :end~%)~%")
    (fmt (tmpl "(loop when ready collect x else collect y end~%)"))
  )
  (check=
    "loop hash clauses normalized"
    (tmpl "(loop~%  :for k~%  :being~%  :the~%  :hash-keys~%  :of table~%  :using value~%)~%")
    (fmt (tmpl "(loop for k being the hash-keys of table using value~%)"))
  )
  (check=
    "loop initially finally normalized"
    (tmpl "(loop~%  :initially (setup)~%  :finally (finish)~%)~%")
    (fmt (tmpl "(loop initially (setup) finally (finish)~%)"))
  )
  (check=
    "loop nested payload not normalized"
    (tmpl "(loop~%  :for x~%  :in (list for in collect)~%  :collect (make for)~%)~%")
    (fmt (tmpl "(loop for x in (list for in collect) collect (make for)~%)"))
  )
  (check=
    "loop it payload not normalized"
    (tmpl "(loop~%  :for it~%  :in xs~%  :collect it~%)~%")
    (fmt (tmpl "(loop~%  for it in xs collect it~%)"))
  )
  (check=
    "loop named payload not normalized"
    (tmpl "(loop~%  :for named~%  :in xs~%  :collect named~%)~%")
    (fmt (tmpl "(loop~%  for named in xs collect named~%)"))
  )
  (check=
    "loop for payload not normalized"
    (tmpl "(loop~%  :for for~%  :in xs~%  :collect for~%)~%")
    (fmt (tmpl "(loop~%  for for in xs collect for~%)"))
  )
  (check=
    "loop typed with assignment"
    (tmpl "(loop~%  :with x fixnum = 1~%  :collect x~%)~%")
    (fmt (tmpl "(loop~%  with x fixnum = 1 collect x~%)"))
  )
  (check=
    "loop with assignment"
    (tmpl "(loop~%  :with x = 1~%  :collect x~%)~%")
    (fmt (tmpl "(loop~%  with x = 1 collect x~%)"))
  )
  (check=
    "loop typed for from"
    (tmpl "(loop~%  :for i fixnum~%  :from 0~%  :collect i~%)~%")
    (fmt (tmpl "(loop~%  for i fixnum from 0 collect i~%)"))
  )
  (check=
    "loop of-type for"
    (tmpl "(loop~%  :for x :of-type fixnum~%  :in xs~%  :collect x~%)~%")
    (fmt (tmpl "(loop~%  for x of-type fixnum in xs collect x~%)"))
  )
  (check=
    "loop typed collision payload"
    (tmpl "(loop~%  :for it fixnum~%  :in xs~%  :collect it~%)~%")
    (fmt (tmpl "(loop~%  for it fixnum in xs collect it~%)"))
  )
  (check=
    "loop named with"
    (tmpl "(loop~%  :named scan~%  :with x = 1~%  :collect x~%)~%")
    (fmt (tmpl "(loop~%  named scan with x = 1 collect x~%)"))
  )
  (check=
    "loop named for"
    (tmpl "(loop~%  :named scan~%  :for x~%  :in xs~%  :collect x~%)~%")
    (fmt (tmpl "(loop~%  named scan for x in xs collect x~%)"))
  )
  (check= "prefix hash chain" (tmpl "###one two~%") (fmt "# # # one two"))
  (check= "prefix quote list" (tmpl "#'(one two)~%") (fmt "# ' ( one two )"))
  (check= "backquote atom number" (tmpl "`123~%") (fmt "`123"))
  (check= "backquote atom symbol" (tmpl "`one~%") (fmt "`one"))
  (check= "backquote string" (tmpl "`\"str\"~%") (fmt "` \"str\""))
  (check= "backquote list" (tmpl "`(one two)~%") (fmt "`(one two)"))
  (check= "backquote newline list" (tmpl "`(one two)~%") (fmt (tmpl "`~%(one two)")))
  (check= "hash backquote list" (tmpl "#`(one two)~%") (fmt "# ` (one two)"))
  (check= "datum comment prefix atom" (tmpl "#;one~%") (fmt "# ; one"))
  (check= "datum comment prefix list" (tmpl "#;(one two)~%") (fmt "# ; ( one two )"))
  (check= "feature prefix plus atom" (tmpl "#+one~%") (fmt "# + one"))
  (check= "feature prefix minus atom" (tmpl "#-one~%") (fmt "# - one"))
  (check= "feature prefix plus list" (tmpl "#+(one two)~%") (fmt "# + (one two)"))
  (check= "fuzzy character literal single char" (tmpl "#\\A~%") (fmt "# \\ A"))
  (check= "fuzzy character literal named char" (tmpl "#\\space~%") (fmt "# \\ space"))
  (check=
    "prefix separated from block comment"
    (tmpl "#':~%#|~%one~%|#~%")
    (fmt "# ' : #|one|#")
  )
  (check=
    "nested block comment"
    (tmpl "#|~%one~%#|~%two~%|#~%three~%|#~%")
    (fmt "#|one#|two|#three|#")
  )
  (check=
    "block comment preserves inner indentation"
    (tmpl "#|~%  one~%    two~%|#~%")
    (fmt (tmpl "#|  one~%    two|#"))
  )
  (check=
    "block comment in list indented"
    (tmpl "(one~%  #|~%  a~%  b~%  |#~%  two~%)~%")
    (fmt (tmpl "(one~%  #|a~%b|#~%  two~%)"))
  )
  (check=
    "nested block comment in list indented"
    (tmpl "(one~%  #|~%  a~%  #|~%  b~%  |#~%  c~%  |#~%  two~%)~%")
    (fmt "(one #|a#|b|#c|# two)")
  )
  (check= "cons dot single line" (tmpl "(one . two)~%") (fmt "(one . two)"))
  (check=
    "cons dot multiline"
    (tmpl "(one~%  .~%  two~%)~%")
    (fmt (tmpl "(one .~%two)"))
  )
  (check=
    "line comment trailing"
    (tmpl "(one ; two~%)~%")
    (fmt (tmpl "(~%one ; two~%)"))
  )
  (check=
    "line comment standalone"
    (tmpl "(~%  one~%  ; two~%)~%")
    (fmt (tmpl "(~%one~%; two~%)"))
  )
  (check=
    "vector multiline"
    (tmpl "#(~%  one~%  two~%  three~%)~%")
    (fmt (tmpl "#(one two~%three~%)"))
  )
  (check=
    "quoted list multiline"
    (tmpl "'(one~%  two~%  three~%)~%")
    (fmt (tmpl "'(one two~%three~%)"))
  )
  (check=
    "full opening token alignment"
    (tmpl "#'#':#(~%  one~%  two~%)~%")
    (fmt (tmpl "#'#':#(one two~%)"))
  )
  (check=
    "multiline string closing quote column zero"
    (tmpl "(print \"one~%two~%\"~%)~%")
    (fmt (tmpl "(print \"one~%two~%  \"~%)"))
  )
  (check= "blank lines trimmed" (tmpl "one~%~%two~%") (fmt (tmpl "one~%~%~%~%two")))
  (check-error "mismatched delimiter" (lambda () (fmt "(]")))
  (check-error "unexpected close" (lambda () (fmt ")")))
  (check-error "whitespace after character literal" (lambda () (fmt "#\\ ")))
  (check-error "fuzzy character literal missing char" (lambda () (fmt "# \\")))
  (let
    ((samples (list "(one . two)" (tmpl "(~%one~%; two~%)") "#|one#|two|#three|#" "# + one" "# - one" "` \"str\"" (tmpl "`~%(one two)") "# ` (one two)" "# \\ space" (tmpl "#|  one~%    two|#") (tmpl "(defmacro with-thing (x)~%body)") (tmpl "(define~%  my-var~%  (+ one two)~%)") (tmpl "(define~%  my-var~%  (+ one~%    two~%  )~%)") (tmpl "(define (square x)~%(* x x))") (tmpl "(defmethod render~%:around~%((x thing))~%body)") (tmpl "(dict~%  :one 10 :two 20 :three 30~%)") (tmpl "(dict~%  :one :two :three :four~%)") (tmpl "(loop~%  :from 0 :for previous = nil :then form :do ...~%)") (tmpl "(loop for form in forms~%      do form)") (tmpl "(loop cl-user::for x cl-user::in xs~%)") (tmpl "(loop~%  for it in xs collect it~%)") (tmpl "(loop~%  for i fixnum from 0 collect i~%)") (tmpl "(loop~%  named scan with x = 1 collect x~%)") (tmpl "(one #|a#|b|#c|# two)") (tmpl "#'#':#(one two~%)"))))
    (dolist
      (sample samples)
      (let
        ((once (fmt sample)))
        (check= (format nil "idempotent ~A" sample) once (fmt once))
      )
    )
  )
  (format t "~&~D failure(s).~%" *failures*)
  (when
    (plusp *failures*)
    (sb-ext:exit :code 1)
  )
)

(run-tests)
