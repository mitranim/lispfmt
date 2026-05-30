(load (merge-pathnames "lispfmt.lisp" *load-truename*))

(defpackage #:lispfmt-cli
  (:use #:cl))

(in-package #:lispfmt-cli)

(defun read-all (stream)
  (with-output-to-string (out)
    (loop for line = (read-line stream nil nil)
          while line
          do (write-string line out)
             (write-char #\Newline out))))

(defun command-line-arguments ()
  (rest sb-ext:*posix-argv*))

(defun main ()
  (handler-case
      (progn
        (when (command-line-arguments)
          (format *error-output* "[lispfmt] arguments are not supported~%")
          (sb-ext:exit :code 2))
        (write-string (lispfmt:format-string (read-all *standard-input*))
                      *standard-output*)
        (finish-output *standard-output*)
        (sb-ext:exit :code 0))
    (sb-sys:interactive-interrupt ()
      (sb-ext:exit :code 130))
    (lispfmt:formatter-error (condition)
      (format *error-output* "[lispfmt] ~A~%" condition)
      (finish-output *error-output*)
      (sb-ext:exit :code 1))
    (error (condition)
      (format *error-output* "[lispfmt] unexpected error: ~A~%" condition)
      (finish-output *error-output*)
      (sb-ext:exit :code 2))))

(defun save-executable ()
  (sb-ext:save-lisp-and-die
   "lispfmt"
   :toplevel #'main
   :executable t
   :save-runtime-options t))

(if (sb-ext:posix-getenv "LISPFMT_BUILD")
    (save-executable)
    (main))
