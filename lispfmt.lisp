(defpackage #:lispfmt
  (:use #:cl)
  (:export #:format-string
           #:formatter-error))

(in-package #:lispfmt)

(defconstant +indent+  2)

(define-condition formatter-error (error)
  ((message :initarg :message :reader formatter-error-message))
  (:report (lambda (condition stream)
             (format stream "~A" (formatter-error-message condition)))))

(defstruct node
  kind
  text
  children
  opener
  closer
  prefix
  start-line
  end-line
  start-col)

(defstruct parser
  text
  (pos 0)
  (line 1)
  (col 0))

(defun normalize-input (string)
  (with-output-to-string (out)
    (loop for i from 0 below (length string)
          for ch = (char string i)
          do (cond
               ((char= ch #\Return)
                (write-char #\Newline out)
                (when (and (< (1+ i) (length string))
                           (char= (char string (1+ i)) #\Newline))
                  (incf i)))
               (t
                (write-char ch out))))))

(defun parser-end-p (p)
  (>= (parser-pos p) (length (parser-text p))))

(defun peek (p &optional (offset 0))
  (let ((index (+ (parser-pos p) offset)))
    (and (< index (length (parser-text p)))
         (char (parser-text p) index))))

(defun starts-with-p (p needle)
  (let ((pos (parser-pos p))
        (text (parser-text p)))
    (and (<= (+ pos (length needle)) (length text))
         (string= needle text :start2 pos :end2 (+ pos (length needle))))))

(defun advance (p)
  (let ((ch (peek p)))
    (unless ch
      (error 'formatter-error :message "Unexpected end of input."))
    (incf (parser-pos p))
    (if (char= ch #\Newline)
        (progn
          (incf (parser-line p))
          (setf (parser-col p) 0))
        (incf (parser-col p)))
    ch))

(defun whitespace-char-p (ch)
  (and ch (member ch '(#\Space #\Tab #\Newline #\Page #\Return) :test #'char=)))

(defun skip-whitespace (p)
  (let ((newlines 0))
    (loop while (and (not (parser-end-p p))
                     (whitespace-char-p (peek p)))
          do (when (char= (peek p) #\Newline)
               (incf newlines))
             (advance p))
    newlines))

(defun opener-p (ch)
  (and ch (find ch "([{")))

(defun closer-p (ch)
  (and ch (find ch ")]}")))

(defun matching-closer (opener)
  (ecase opener
    (#\( #\))
    (#\[ #\])
    (#\{ #\})))

(defun delimiter-char-p (ch)
  (or (opener-p ch) (closer-p ch)))

(defun word-char-p (ch)
  (and ch (or (alphanumericp ch)
              (member ch '(#\- #\_) :test #'char=))))

(defun parse-line-comment (p)
  (let ((start-line (parser-line p))
        (start-col (parser-col p))
        (out (make-string-output-stream)))
    (loop while (and (not (parser-end-p p))
                     (not (char= (peek p) #\Newline)))
          do (write-char (advance p) out))
    (make-node :kind :line-comment
               :text (get-output-stream-string out)
               :start-line start-line
               :end-line (parser-line p)
               :start-col start-col)))

(defun parse-string-node (p)
  (let ((start-line (parser-line p))
        (start-col (parser-col p))
        (escaped nil)
        (out (make-string-output-stream)))
      (write-char (advance p) out)
      (loop while (not (parser-end-p p))
            for ch = (advance p)
            do (write-char ch out)
               (cond
                 (escaped (setf escaped nil))
                 ((char= ch #\\) (setf escaped t))
                 ((char= ch #\") (return)))
            finally (when (parser-end-p p)
                      (error 'formatter-error :message "Unterminated string.")))
      (make-node :kind :string
                 :text (get-output-stream-string out)
                 :start-line start-line
                 :end-line (parser-line p)
                 :start-col start-col)))

(defun parse-block-comment (p)
  (let ((start-line (parser-line p))
        (start-col (parser-col p))
        (depth 0)
        (out (make-string-output-stream)))
      (loop while (not (parser-end-p p))
            do (cond
                 ((starts-with-p p "#|")
                  (incf depth)
                  (write-char (advance p) out)
                  (write-char (advance p) out))
                 ((starts-with-p p "|#")
                  (decf depth)
                  (write-char (advance p) out)
                  (write-char (advance p) out)
                  (when (zerop depth)
                    (return)))
                 (t
                  (write-char (advance p) out))))
      (unless (zerop depth)
        (error 'formatter-error :message "Unterminated block comment."))
      (make-node :kind :block-comment
                 :text (get-output-stream-string out)
                 :start-line start-line
                 :end-line (parser-line p)
                 :start-col start-col)))

(defun parse-character-literal-tail (p start-line start-col)
  (let ((next (peek p)))
    (when (or (null next) (whitespace-char-p next))
      (error 'formatter-error :message "Character literal requires a visible character or name.")))
    (let ((out (make-string-output-stream)))
      (write-string "#\\" out)
      (if (word-char-p (peek p))
          (loop while (word-char-p (peek p))
                do (write-char (advance p) out))
          (write-char (advance p) out))
      (make-node :kind :atom
                 :text (get-output-stream-string out)
                 :start-line start-line
                 :end-line (parser-line p)
                 :start-col start-col)))

(defun parse-character-literal (p)
  (let ((start-line (parser-line p))
        (start-col (parser-col p)))
    (advance p)
    (advance p)
    (parse-character-literal-tail p start-line start-col)))

(defun parse-atom (p)
  (let ((start-line (parser-line p))
        (start-col (parser-col p))
        (out (make-string-output-stream)))
      (loop while (and (not (parser-end-p p))
                       (not (whitespace-char-p (peek p)))
                       (not (delimiter-char-p (peek p)))
                       (not (char= (peek p) #\;))
                       (not (char= (peek p) #\")))
            do (when (starts-with-p p "#|")
                 (return))
               (write-char (advance p) out))
      (let ((text (get-output-stream-string out)))
      (when (zerop (length text))
        (error 'formatter-error :message "Expected atom."))
      (make-node :kind :atom
                 :text text
                 :start-line start-line
                 :end-line (parser-line p)
                 :start-col start-col))))

(defun prefix-token (p &optional have-prefix)
  (let ((ch (peek p)))
    (cond
      ((null ch) nil)
      ((char= ch #\#)
       (cond
         ((starts-with-p p "#|") nil)
         ((starts-with-p p "#\\") nil)
         (t "#")))
      ((member ch '(#\' #\` #\,) :test #'char=)
       (string ch))
      ((and have-prefix (member ch '(#\+ #\- #\\) :test #'char=))
       (string ch))
      ((and have-prefix (char= ch #\;))
       ";")
      (t nil))))

(defun parse-prefix-chain (p)
  (let ((parts '())
        (start-line (parser-line p))
        (start-col (parser-col p)))
    (loop
      (skip-whitespace p)
      (let ((token (prefix-token p parts)))
        (unless token
          (return))
        (push token parts)
        (dotimes (i (length token))
          (declare (ignorable i))
          (advance p))))
    (when parts
      (let ((prefix (apply #'concatenate 'string (nreverse parts))))
        (skip-whitespace p)
        (when (and (>= (length prefix) 2)
                   (string= "#\\" prefix
                            :start1 (- (length prefix) 2)))
          (return-from parse-prefix-chain
            (parse-character-literal-tail p start-line start-col)))
        (when (and (not (parser-end-p p))
                   (char= (peek p) #\:))
          (setf prefix (concatenate 'string prefix ":"))
          (advance p)
          (skip-whitespace p))
        (when (or (parser-end-p p)
                  (and (starts-with-p p "#|") t))
          (return-from parse-prefix-chain
            (make-node :kind :prefix
                       :text prefix
                       :start-line start-line
                       :end-line start-line
                       :start-col start-col)))
        (let ((child (parse-form p)))
          (make-node :kind :prefix
                     :text prefix
                     :children (list child)
                     :start-line start-line
                     :end-line (if (and (member (node-kind child) '(:list :vector))
                                        (= (node-start-line child) (node-end-line child)))
                                   start-line
                                   (node-end-line child))
                     :start-col start-col))))))

(defun parse-list (p &optional prefix start-line start-col)
  (let* ((opener (advance p))
         (closer (matching-closer opener))
         (children '())
         (line (or start-line (parser-line p)))
         (col (or start-col (1- (parser-col p)))))
    (loop
      (skip-whitespace p)
      (when (parser-end-p p)
        (error 'formatter-error :message "Unclosed delimiter."))
      (when (closer-p (peek p))
        (let ((actual (advance p)))
          (unless (char= actual closer)
            (error 'formatter-error
                   :message (format nil "Mismatched delimiter ~C, expected ~C." actual closer))))
        (return))
      (push (parse-form p) children))
    (let ((node (make-node :kind :list
                           :children (nreverse children)
                           :opener (string opener)
                           :closer (string closer)
                           :prefix prefix
                           :start-line line
                           :end-line (parser-line p)
                           :start-col col)))
      (setf (node-kind node)
            (if (and prefix
                     (plusp (length prefix))
                     (char= (char prefix (1- (length prefix))) #\#))
                :vector
                :list))
      node)))

(defun parse-form (p)
  (skip-whitespace p)
  (when (parser-end-p p)
    (error 'formatter-error :message "Expected form."))
  (let ((prefix (parse-prefix-chain p)))
    (when prefix
      (let ((child (first (node-children prefix))))
        (when (and child (member (node-kind child) '(:list :vector)))
          (let ((child-single-line-p (= (node-start-line child) (node-end-line child))))
            (setf (node-prefix child) (concatenate 'string (node-text prefix) (or (node-prefix child) ""))
                  (node-start-line child) (node-start-line prefix)
                  (node-end-line child) (if child-single-line-p
                                            (node-start-line prefix)
                                            (node-end-line child))
                  (node-start-col child) (node-start-col prefix)))
          (when (and (plusp (length (node-prefix child)))
                     (char= (char (node-prefix child)
                                  (1- (length (node-prefix child))))
                            #\#))
            (setf (node-kind child) :vector))
          (return-from parse-form child)))
      (return-from parse-form prefix)))
  (cond
    ((closer-p (peek p))
     (error 'formatter-error :message "Unexpected closing delimiter."))
    ((opener-p (peek p))
     (parse-list p))
    ((starts-with-p p "#|")
     (parse-block-comment p))
    ((starts-with-p p "#\\")
     (parse-character-literal p))
    ((char= (peek p) #\;)
     (parse-line-comment p))
    ((char= (peek p) #\")
     (parse-string-node p))
    (t
     (parse-atom p))))

(defun parse-all (string)
  (let ((p (make-parser :text (normalize-input string)))
        (forms '())
        (gaps '()))
    (loop
      (let ((gap (skip-whitespace p)))
        (when (parser-end-p p)
          (return))
        (when (closer-p (peek p))
          (error 'formatter-error :message "Unexpected closing delimiter."))
        (push (min gap 2) gaps)
        (push (parse-form p) forms)))
    (values (nreverse forms) (nreverse gaps))))

(defun trim-right-spaces (string)
  (string-right-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun split-lines (string)
  (let ((lines '())
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) #\Newline)
            do (push (subseq string start i) lines)
               (setf start (1+ i)))
    (push (subseq string start) lines)
    (nreverse lines)))

(defun ensure-comment-space (comment)
  (let* ((trimmed (trim-right-spaces comment))
         (len (length trimmed))
         (i 0))
    (loop while (and (< i len) (char= (char trimmed i) #\;))
          do (incf i))
    (cond
      ((= i len) trimmed)
      ((char= (char trimmed i) #\Space) trimmed)
      (t (concatenate 'string (subseq trimmed 0 i) " " (subseq trimmed i))))))

(defun flush-block-comment-text (out pending)
  (let ((text (string-right-trim
               '(#\Space #\Tab #\Newline #\Return)
               (string-left-trim '(#\Newline #\Return)
                                 (get-output-stream-string pending)))))
    (when (plusp (length text))
      (write-string text out)
      (write-char #\Newline out))))

(defun format-block-comment (text)
  (let ((out (make-string-output-stream))
        (pending (make-string-output-stream))
        (i 0))
    (loop while (< i (length text))
          do (cond
               ((and (<= (+ i 2) (length text))
                     (string= "#|" text :start2 i :end2 (+ i 2)))
                (flush-block-comment-text out pending)
                (write-string "#|" out)
                (write-char #\Newline out)
                (incf i 2))
               ((and (<= (+ i 2) (length text))
                     (string= "|#" text :start2 i :end2 (+ i 2)))
                (flush-block-comment-text out pending)
                (write-string "|#" out)
                (incf i 2)
                (when (< i (length text))
                  (write-char #\Newline out)))
               (t
                (write-char (char text i) pending)
                (incf i))))
    (flush-block-comment-text out pending)
    (string-right-trim '(#\Newline) (get-output-stream-string out))))

(defun format-string-literal (text)
  (if (not (find #\Newline text))
      text
      (let ((lines (split-lines text)))
        (with-output-to-string (out)
          (loop for line in lines
                for index from 0
                do (when (> index 0)
                     (write-char #\Newline out))
                   (cond
                     ((and (= index (1- (length lines)))
                           (string= "\"" (string-left-trim '(#\Space #\Tab) line)))
                      (write-char #\" out))
                     ((every (lambda (ch) (member ch '(#\Space #\Tab) :test #'char=)) line)
                      nil)
                     (t
                      (write-string line out))))))))

(defun callable-atom-p (node)
  (and (eq (node-kind node) :atom)
       (let ((text (node-text node)))
         (and (plusp (length text))
              (not (find (char text 0) "'\":;#0123456789|."))
              (not (search " " text))))))

(defun atom-text= (node text)
  (and (eq (node-kind node) :atom)
       (string-equal (node-text node) text)))

(defun string-prefix-p (prefix string &key (test #'char=))
  (let ((prefix-length (length prefix)))
    (and (<= prefix-length (length string))
         (loop for index below prefix-length
               always (funcall test
                               (char prefix index)
                               (char string index))))))

(defun def-operator-p (node)
  (and (eq (node-kind node) :atom)
       (string-prefix-p "def" (node-text node) :test #'char-equal)))

(defun keyword-atom-p (node)
  (and (eq (node-kind node) :atom)
       (plusp (length (node-text node)))
       (char= (char (node-text node) 0) #\:)))

(defun loop-operator-p (node)
  (atom-text= node "loop"))

(defun definition-name-p (node)
  (or (member (node-kind node) '(:list :vector))
      (and (eq (node-kind node) :atom)
           (callable-atom-p node)
           (not (keyword-atom-p node)))))

(defun definition-head-count (children)
  (when (and (>= (length children) 2)
             (def-operator-p (first children))
             (definition-name-p (second children)))
    (if (and (not (atom-text= (first children) "define"))
             (eq (node-kind (second children)) :atom)
             (third children)
             (member (node-kind (third children)) '(:list :vector)))
        3
        2)))

(defun node-inline-string (node)
  (case (node-kind node)
    (:atom (node-text node))
    (:string (format-string-literal (node-text node)))
    (:line-comment (ensure-comment-space (node-text node)))
    (:block-comment (format-block-comment (node-text node)))
    (:prefix
     (if (node-children node)
         (concatenate 'string (node-text node) (node-inline-string (first (node-children node))))
         (node-text node)))
    ((:list :vector)
     (format-list-node node 0 :inline t))))

(defun multiline-string-p (s)
  (find #\Newline s))

(defun node-forces-multiline-p (node)
  (or (eq (node-kind node) :line-comment)
      (eq (node-kind node) :block-comment)
      (and (eq (node-kind node) :string)
           (multiline-string-p (node-text node)))))

(defun inline-safe-node-p (node)
  (and (not (node-forces-multiline-p node))
       (not (binding-list-p node))
       (not (and (member (node-kind node) '(:list :vector))
                 (list-multiline-p node)))))

(defun list-multiline-p (node)
  (or (/= (node-start-line node) (node-end-line node))
      (some #'node-forces-multiline-p (node-children node))))

(defun binding-list-p (node)
  (and (member (node-kind node) '(:list :vector))
       (node-children node)
       (every (lambda (child)
                (member (node-kind child) '(:list :vector)))
              (node-children node))))

(defun trailing-line-comment-p (previous child)
  (and previous
       (eq (node-kind child) :line-comment)
       (= (node-end-line previous) (node-start-line child))))

(defun has-standalone-line-comment-p (children)
  (loop for previous = nil then child
        for child in children
        thereis (and (eq (node-kind child) :line-comment)
                     (not (trailing-line-comment-p previous child)))))

(defun indent-string (indent)
  (make-string indent :initial-element #\Space))

(defun format-for-child-line (node indent)
  (if (binding-list-p node)
      (format-list-node node indent :force-multiline t)
      (format-node node indent)))

(defun consume-generic-keyword-line (children indent)
  (let ((child (first children))
        (next (second children)))
    (if (and (keyword-atom-p child)
             next
             (inline-safe-node-p next))
        (values (format nil "~A ~A"
                        (format-node child indent)
                        (node-inline-string next))
                (cddr children)
                next)
        (values (format-for-child-line child indent)
                (rest children)
                child))))

(defun consume-loop-keyword-line (children indent)
  (let ((child (first children)))
    (if (keyword-atom-p child)
        (let ((line-nodes (list child))
              (rest (rest children)))
          (loop while (and rest
                           (not (keyword-atom-p (first rest)))
                           (inline-safe-node-p (first rest)))
                do (setf line-nodes (append line-nodes (list (first rest)))
                         rest (rest rest)))
          (values (format-inline-children line-nodes)
                  rest
                  (first (last line-nodes))))
        (values (format-for-child-line child indent)
                (rest children)
                child))))

(defun consume-plain-child-line (children indent)
  (let ((child (first children)))
    (values (format-for-child-line child indent)
            (rest children)
            child)))

(defun write-child-lines (children indent out &key loop-style group-keywords previous)
  (loop while children
        do (multiple-value-bind (line rest last-node)
               (cond
                 (loop-style
                  (consume-loop-keyword-line children indent))
                 (group-keywords
                  (consume-generic-keyword-line children indent))
                 (t
                  (consume-plain-child-line children indent)))
             (if (and previous
                      (= (node-end-line previous) (node-start-line (first children)))
                      (eq (node-kind (first children)) :line-comment))
                 (progn
                   (write-char #\Space out)
                   (write-string line out))
                 (progn
                   (write-char #\Newline out)
                   (write-string (indent-string indent) out)
                   (write-string line out)))
             (setf previous last-node
                   children rest))))

(defun format-node (node indent)
  (case (node-kind node)
    (:atom (node-text node))
    (:string (format-string-literal (node-text node)))
    (:line-comment (ensure-comment-space (node-text node)))
    (:block-comment (format-block-comment (node-text node)))
    (:prefix
     (if (node-children node)
         (let ((child (first (node-children node))))
           (if (eq (node-kind child) :block-comment)
               (format nil "~A~%~A" (node-text node) (format-node child indent))
               (concatenate 'string (node-text node) (format-node child indent))))
         (node-text node)))
    ((:list :vector)
     (format-list-node node indent))))

(defun format-list-node (node indent &key inline force-multiline)
  (let* ((prefix (or (node-prefix node) ""))
         (open-token (concatenate 'string prefix (node-opener node)))
         (children (node-children node))
         (close (node-closer node)))
    (cond
      ((null children)
       (concatenate 'string open-token close))
      ((and (not force-multiline)
            (or inline
                (not (list-multiline-p node)))
            (notany #'node-forces-multiline-p children))
       (concatenate 'string open-token
                    (format-inline-children children)
                    close))
      ((and (= (length children) 1)
            (notany #'node-forces-multiline-p children))
       (concatenate 'string open-token
                    (node-inline-string (first children))
                    close))
      ((and (eq (node-kind node) :list)
            (definition-head-count children))
       (let ((head-count (definition-head-count children)))
         (let ((remaining (nthcdr head-count children)))
           (if (and (= (length remaining) 1)
                    (inline-safe-node-p (first remaining)))
               (concatenate 'string
                            open-token
                            (format-inline-children (subseq children 0 head-count))
                            " "
                            (node-inline-string (first remaining))
                            close)
               (with-output-to-string (out)
                 (write-string open-token out)
                 (write-string (format-inline-children (subseq children 0 head-count)) out)
                 (dolist (child remaining)
                   (write-char #\Newline out)
                   (write-string (indent-string (+ indent +indent+)) out)
                   (write-string (format-node child (+ indent +indent+)) out))
                 (write-char #\Newline out)
                 (write-string (indent-string indent) out)
                 (write-string close out))))))
      ((and (eq (node-kind node) :list)
            (not (has-standalone-line-comment-p children))
            (callable-atom-p (first children)))
       (with-output-to-string (out)
         (write-string open-token out)
         (let ((previous (first children))
               (remaining (rest children)))
           (write-string (format-node previous (+ indent (length open-token))) out)
           (when (and (first remaining)
                      (eq (node-kind (first remaining)) :string)
                      (multiline-string-p (node-text (first remaining))))
             (write-char #\Space out)
             (write-string (format-node (first remaining) (+ indent (length open-token) 1)) out)
             (setf previous (first remaining)
                   remaining (rest remaining)))
           (write-child-lines remaining
                              (+ indent +indent+)
                              out
                              :loop-style (loop-operator-p (first children))
                              :group-keywords (not (def-operator-p (first children)))
                              :previous previous))
         (write-char #\Newline out)
         (write-string (indent-string indent) out)
         (write-string close out)))
      (t
       (with-output-to-string (out)
         (write-string open-token out)
         (write-child-lines children (+ indent +indent+) out :group-keywords t)
         (write-char #\Newline out)
         (write-string (indent-string indent) out)
         (write-string close out))))))

(defun format-inline-children (children)
  (with-output-to-string (out)
    (loop for child in children
          for index from 0
          do (when (> index 0)
               (write-char #\Space out))
             (write-string (node-inline-string child) out))))

(defun normalize-final-newlines (string)
  (concatenate 'string (string-right-trim '(#\Newline #\Space #\Tab) string) (string #\Newline) (string #\Newline)))

(defun format-string (string)
  (multiple-value-bind (forms gaps) (parse-all string)
    (let ((raw (make-string-output-stream)))
      (loop for form in forms
            for gap in gaps
            for index from 0
            for previous = nil then form
            do (when (> index 0)
                 (if (and (zerop gap)
                          (not (eq (node-kind form) :block-comment))
                          (not (and previous
                                    (eq (node-kind previous) :prefix)
                                    (null (node-children previous)))))
                     (write-char #\Space raw)
                     (progn
                       (write-char #\Newline raw)
                       (when (>= gap 2)
                         (write-char #\Newline raw)))))
               (write-string (format-node form 0) raw))
      (normalize-final-newlines (get-output-stream-string raw)))))
