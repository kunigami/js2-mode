;; Helper functions
(defun js2-delete-if (predicate list)
  "Remove all items satisfying PREDICATE in LIST."
  (loop for item in list
        if (not (funcall predicate item))
        collect item))

(defun js2-position (element list)
  "Find 0-indexed position of ELEMENT in LIST comparing with `eq'.
Returns nil if element is not found in the list."
  (let ((count 0)
        found)
    (while (and list (not found))
      (if (eq element (car list))
          (setq found t)
        (setq count (1+ count)
              list (cdr list))))
    (if found count)))

(defun js2-find-if (predicate list)
  "Find first item satisfying PREDICATE in LIST."
  (let (result)
    (while (and list (not result))
      (if (funcall predicate (car list))
          (setq result (car list)))
      (setq list (cdr list)))
    result))

(defmacro js2-time (form)
  "Evaluate FORM, discard result, and return elapsed time in sec."
  (declare (debug t))
  (let ((beg (make-symbol "--js2-time-beg--"))
        (delta (make-symbol "--js2-time-end--")))
    `(let ((,beg (current-time))
           ,delta)
       ,form
       (/ (truncate (* (- (float-time (current-time))
                          (float-time ,beg))
                       10000))
          10000.0))))

(defsubst js2-same-line (pos)
  "Return t if POS is on the same line as current point."
  (and (>= pos (point-at-bol))
       (<= pos (point-at-eol))))

(defun js2-code-bug ()
  "Signal an error when we encounter an unexpected code path."
  (error "failed assertion"))

(defsubst js2-record-text-property (beg end prop value)
  "Record a text property to set when parsing finishes."
  (push (list beg end prop value) js2-mode-deferred-properties))

;; I'd like to associate errors with nodes, but for now the
;; easiest thing to do is get the context info from the last token.
(defun js2-record-parse-error (msg &optional arg pos len)
  (push (list (list msg arg)
              (or pos (js2-current-token-beg))
              (or len (js2-current-token-len)))
        js2-parsed-errors))

(defun js2-report-error (msg &optional msg-arg pos len)
  "Signal a syntax error or record a parse error."
  (if js2-recover-from-parse-errors
      (js2-record-parse-error msg msg-arg pos len)
  (signal 'js2-syntax-error
          (list msg
                js2-ts-lineno
                (save-excursion
                  (goto-char js2-ts-cursor)
                  (current-column))
                js2-ts-hit-eof))))

(defun js2-report-warning (msg &optional msg-arg pos len face)
  (if js2-compiler-report-warning-as-error
      (js2-report-error msg msg-arg pos len)
    (push (list (list msg msg-arg)
                (or pos (js2-current-token-beg))
                (or len (js2-current-token-len))
                face)
          js2-parsed-warnings)))

(defun js2-add-strict-warning (msg-id &optional msg-arg beg end)
  (if js2-compiler-strict-mode
      (js2-report-warning msg-id msg-arg beg
                          (and beg end (- end beg)))))

(put 'js2-syntax-error 'error-conditions
     '(error syntax-error js2-syntax-error))
(put 'js2-syntax-error 'error-message "Syntax error")

(put 'js2-parse-error 'error-conditions
     '(error parse-error js2-parse-error))
(put 'js2-parse-error 'error-message "Parse error")

(defmacro js2-clear-flag (flags flag)
  `(setq ,flags (logand ,flags (lognot ,flag))))

(defmacro js2-set-flag (flags flag)
  "Logical-or FLAG into FLAGS."
  `(setq ,flags (logior ,flags ,flag)))

(defsubst js2-flag-set-p (flags flag)
  (/= 0 (logand flags flag)))

(defsubst js2-flag-not-set-p (flags flag)
  (zerop (logand flags flag)))

(defmacro js2-with-underscore-as-word-syntax (&rest body)
  "Evaluate BODY with the _ character set to be word-syntax."
  (declare (indent 0) (debug t))
  (let ((old-syntax (make-symbol "old-syntax")))
  `(let ((,old-syntax (string (char-syntax ?_))))
     (unwind-protect
         (progn
           (modify-syntax-entry ?_ "w" js2-mode-syntax-table)
           ,@body)
       (modify-syntax-entry ?_ ,old-syntax js2-mode-syntax-table)))))

(defsubst js2-char-uppercase-p (c)
  "Return t if C is an uppercase character.
Handles unicode and latin chars properly."
  (/= c (downcase c)))

(defsubst js2-char-lowercase-p (c)
  "Return t if C is an uppercase character.
Handles unicode and latin chars properly."
  (/= c (upcase c)))

(provide 'js2-utils)
