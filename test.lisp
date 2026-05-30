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
  (check= "prefix hash chain" (s "###one two~%~%") (fmt "# # # one two"))
  (check= "prefix quote list" (s "#'(one two)~%~%") (fmt "# ' ( one two )"))
  (check= "datum comment prefix atom" (s "#;one~%~%") (fmt "# ; one"))
  (check= "datum comment prefix list" (s "#;(one two)~%~%") (fmt "# ; ( one two )"))
  (check=
   "prefix separated from block comment"
   (s "#':~%#|~%one~%|#~%~%")
   (fmt "# ' : #|one|#"))
  (check=
   "nested block comment"
   (s "#|~%one~%#|~%two~%|#~%three~%|#~%~%")
   (fmt "#|one#|two|#three|#"))
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
  (let ((samples (list "(one . two)"
                       (s "(~%one~%; two~%)")
                       "#|one#|two|#three|#"
                       (s "#'#':#(one two~%)"))))
    (dolist (sample samples)
      (let ((once (fmt sample)))
        (check= (format nil "idempotent ~A" sample) once (fmt once)))))
  (format t "~&~D failure(s).~%" *failures*)
  (when (plusp *failures*)
    (sb-ext:exit :code 1)))

(run-tests)
