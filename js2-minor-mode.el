;; To be used as a linter with other js modes
(defvar js2-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-`") #'js2-next-error)
    (define-key map [mouse-1] #'js2-mode-show-node)
    map)
  "Keymap used when `js2-minor-mode' is active.")

;;;###autoload
(define-minor-mode js2-minor-mode
  "Minor mode for running js2 as a background linter.
This allows you to use a different major mode for JavaScript editing,
such as `espresso-mode', while retaining the asynchronous error/warning
highlighting features of `js2-mode'."
  :group 'js2-mode
  :lighter " js-lint"
  (if js2-minor-mode
      (js2-minor-mode-enter)
    (js2-minor-mode-exit)))

(defun js2-minor-mode-enter ()
  "Initialization for `js2-minor-mode'."
  (set (make-local-variable 'max-lisp-eval-depth)
       (max max-lisp-eval-depth 3000))
  (setq next-error-function #'js2-next-error)
  (js2-set-default-externs)
  ;; Experiment:  make reparse-delay longer for longer files.
  (if (plusp js2-dynamic-idle-timer-adjust)
      (setq js2-idle-timer-delay
            (* js2-idle-timer-delay
               (/ (point-max) js2-dynamic-idle-timer-adjust))))
  (setq js2-mode-buffer-dirty-p t
        js2-mode-parsing nil)
  (set (make-local-variable 'js2-highlight-level) 0) ; no syntax highlighting
  (add-hook 'after-change-functions #'js2-minor-mode-edit nil t)
  (add-hook 'change-major-mode-hook #'js2-minor-mode-exit nil t)
  (when js2-include-jslint-globals
    (add-hook 'js2-post-parse-callbacks 'js2-apply-jslint-globals nil t))
  (run-hooks 'js2-init-hook)
  (js2-reparse))

(defun js2-minor-mode-exit ()
  "Turn off `js2-minor-mode'."
  (setq next-error-function nil)
  (remove-hook 'after-change-functions #'js2-mode-edit t)
  (remove-hook 'change-major-mode-hook #'js2-minor-mode-exit t)
  (when js2-mode-node-overlay
    (delete-overlay js2-mode-node-overlay)
    (setq js2-mode-node-overlay nil))
  (js2-remove-overlays)
  (remove-hook 'js2-post-parse-callbacks 'js2-apply-jslint-globals t)
  (setq js2-mode-ast nil))

(provide 'js2-minor-mode)
