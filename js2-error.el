(defun js2-errors ()
  "Return a list of errors found."
  (and js2-mode-ast
       (js2-ast-root-errors js2-mode-ast)))

(defun js2-warnings ()
  "Return a list of warnings found."
  (and js2-mode-ast
       (js2-ast-root-warnings js2-mode-ast)))

(defun js2-have-errors-p ()
  "Return non-nil if any parse errors or warnings were found."
  (or (js2-errors) (js2-warnings)))

(defun js2-errors-and-warnings ()
  "Return a copy of the concatenated errors and warnings lists.
They are appended:  first the errors, then the warnings.
Entries are of the form (MSG BEG END)."
  (when js2-mode-ast
    (append (js2-ast-root-errors js2-mode-ast)
            (copy-sequence (js2-ast-root-warnings js2-mode-ast)))))

(defun js2-next-error (&optional arg reset)
  "Move to next parse error.
Typically invoked via \\[next-error].
ARG is the number of errors, forward or backward, to move.
RESET means start over from the beginning."
  (interactive "p")
  (if (not (or (js2-errors) (js2-warnings)))
      (message "No errors")
    (when reset
      (goto-char (point-min)))
    (let* ((errs (js2-errors-and-warnings))
           (continue t)
           (start (point))
           (count (or arg 1))
           (backward (minusp count))
           (sorter (if backward '> '<))
           (stopper (if backward '< '>))
           (count (abs count))
           all-errs err)
      ;; Sort by start position.
      (setq errs (sort errs (lambda (e1 e2)
                              (funcall sorter (second e1) (second e2))))
            all-errs errs)
      ;; Find nth error with pos > start.
      (while (and errs continue)
        (when (funcall stopper (cadar errs) start)
          (setq err (car errs))
          (if (zerop (decf count))
              (setq continue nil)))
        (setq errs (cdr errs)))
      ;; Clear for `js2-echo-error'.
      (message nil)
      (if err
          (goto-char (second err))
        ;; Wrap around to first error.
        (goto-char (second (car all-errs)))
        ;; If we were already on it, echo msg again.
        (if (= (point) start)
            (js2-echo-error (point) (point)))))))

(provide 'js2-error)
