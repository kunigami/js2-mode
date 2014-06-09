;; This indenter is based on Karl Landström's "javascript.el" indenter.
;; Karl cleverly deduces that the desired indentation level is often a
;; function of paren/bracket/brace nesting depth, which can be determined
;; quickly via the built-in `parse-partial-sexp' function.  His indenter
;; then does some equally clever checks to see if we're in the context of a
;; substatement of a possibly braceless statement keyword such as if, while,
;; or finally.  This approach yields pretty good results.

;; The indenter is often "wrong", however, and needs to be overridden.
;; The right long-term solution is probably to emulate (or integrate
;; with) cc-engine, but it's a nontrivial amount of coding.  Even when a
;; parse tree from `js2-parse' is present, which is not true at the
;; moment the user is typing, computing indentation is still thousands
;; of lines of code to handle every possible syntactic edge case.

;; In the meantime, the compromise solution is that we offer a "bounce
;; indenter", configured with `js2-bounce-indent-p', which cycles the
;; current line indent among various likely guess points.  This approach
;; is far from perfect, but should at least make it slightly easier to
;; move the line towards its desired indentation when manually
;; overriding Karl's heuristic nesting guesser.

;; I've made miscellaneous tweaks to Karl's code to handle some Ecma
;; extensions such as `let' and Array comprehensions.  Major kudos to
;; Karl for coming up with the initial approach, which packs a lot of
;; punch for so little code.

(defconst js2-possibly-braceless-keywords-re
  (concat "else[ \t]+if\\|for[ \t]+each\\|"
          (regexp-opt '("catch" "do" "else" "finally" "for" "if"
                        "try" "while" "with" "let")))
  "Regular expression matching keywords that are optionally
followed by an opening brace.")

(defconst js2-indent-operator-re
  (concat "[-+*/%<>=&^|?:.]\\([^-+*/]\\|$\\)\\|"
          (regexp-opt '("in" "instanceof") 'words))
  "Regular expression matching operators that affect indentation
of continued expressions.")

(defconst js2-declaration-keyword-re
  (regexp-opt '("var" "let" "const") 'words)
  "Regular expression matching variable declaration keywords.")

(defun js2-re-search-forward-inner (regexp &optional bound count)
  "Auxiliary function for `js2-re-search-forward'."
  (let (parse saved-point)
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (if saved-point
                      (parse-partial-sexp saved-point (point))
                    (syntax-ppss (point))))
      (cond ((nth 3 parse)
             (re-search-forward
              (concat "\\(\\=\\|[^\\]\\|^\\)" (string (nth 3 parse)))
              (save-excursion (end-of-line) (point)) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            (t
             (setq count (1- count))))
      (setq saved-point (point))))
  (point))

(defun js2-re-search-forward (regexp &optional bound noerror count)
  "Search forward but ignore strings and comments.
Invokes `re-search-forward' but treats the buffer as if strings
and comments have been removed."
  (let ((saved-point (point)))
    (condition-case err
        (cond ((null count)
               (js2-re-search-forward-inner regexp bound 1))
              ((< count 0)
               (js2-re-search-backward-inner regexp bound (- count)))
              ((> count 0)
               (js2-re-search-forward-inner regexp bound count)))
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))

(defun js2-re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `js2-re-search-backward'."
  (let (parse)
    (while (> count 0)
      (re-search-backward regexp bound)
      (setq parse (syntax-ppss (point)))
      (cond ((nth 3 parse)
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string (nth 3 parse)))
              (line-beginning-position) t))
            ((nth 7 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            (t
             (setq count (1- count))))))
  (point))

(defun js2-re-search-backward (regexp &optional bound noerror count)
  "Search backward but ignore strings and comments.
Invokes `re-search-backward' but treats the buffer as if strings
and comments have been removed."
  (let ((saved-point (point)))
    (condition-case err
        (cond ((null count)
               (js2-re-search-backward-inner regexp bound 1))
              ((< count 0)
               (js2-re-search-forward-inner regexp bound (- count)))
              ((> count 0)
               (js2-re-search-backward-inner regexp bound count)))
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))

(defun js2-looking-at-operator-p ()
  "Return non-nil if text after point is a non-comma operator."
  (and (looking-at js2-indent-operator-re)
       (or (not (looking-at ":"))
           (save-excursion
             (and (js2-re-search-backward "[?:{]\\|\\<case\\>" nil t)
                  (looking-at "?"))))))

(defun js2-continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (js2-looking-at-operator-p)
        (when (catch 'found
                (while (and (re-search-backward "\n" nil t)
                            (let ((state (syntax-ppss)))
                              (when (nth 4 state)
                                (goto-char (nth 8 state))) ;; skip comments
                              (skip-chars-backward " \t")
                              (if (bolp)
                                  t
                                (throw 'found t))))))
          (backward-char)
          (when (js2-looking-at-operator-p)
            (backward-char)
            (not (looking-at "\\*\\|++\\|--\\|/[/*]")))))))

(defun js2-end-of-do-while-loop-p ()
  "Return non-nil if word after point is `while' of a do-while
statement, else returns nil. A braceless do-while statement
spanning several lines requires that the start of the loop is
indented to the same column as the current line."
  (interactive)
  (save-excursion
    (when (looking-at "\\s-*\\<while\\>")
      (if (save-excursion
            (skip-chars-backward "[ \t\n]*}")
            (looking-at "[ \t\n]*}"))
          (save-excursion
            (backward-list) (backward-word 1) (looking-at "\\<do\\>"))
        (js2-re-search-backward "\\<do\\>" (point-at-bol) t)
        (or (looking-at "\\<do\\>")
            (let ((saved-indent (current-indentation)))
              (while (and (js2-re-search-backward "^[ \t]*\\<" nil t)
                          (/= (current-indentation) saved-indent)))
              (and (looking-at "[ \t]*\\<do\\>")
                   (not (js2-re-search-forward
                         "\\<while\\>" (point-at-eol) t))
                   (= (current-indentation) saved-indent))))))))

(defun js2-multiline-decl-indentation ()
  "Return the declaration indentation column if the current line belongs
to a multiline declaration statement.  See `js2-pretty-multiline-declarations'."
  (let (forward-sexp-function ; use Lisp version
        at-opening-bracket)
    (save-excursion
      (back-to-indentation)
      (when (not (looking-at js2-declaration-keyword-re))
        (when (looking-at js2-indent-operator-re)
          (goto-char (match-end 0))) ; continued expressions are ok
        (while (and (not at-opening-bracket)
                    (not (bobp))
                    (let ((pos (point)))
                      (save-excursion
                        (js2-backward-sws)
                        (or (eq (char-before) ?,)
                            (and (not (eq (char-before) ?\;))
                                 (prog2 (skip-syntax-backward ".")
                                     (looking-at js2-indent-operator-re)
                                   (js2-backward-sws))
                                 (not (eq (char-before) ?\;)))
                            (js2-same-line pos)))))
          (condition-case _
              (backward-sexp)
            (scan-error (setq at-opening-bracket t))))
        (when (looking-at js2-declaration-keyword-re)
          (goto-char (match-end 0))
          (1+ (current-column)))))))

(defun js2-ctrl-statement-indentation ()
  "Return the proper indentation of current line if it is a control statement.
Returns an indentation if this line starts the body of a control
statement without braces, else returns nil."
  (let (forward-sexp-function)
    (save-excursion
      (back-to-indentation)
      (when (and (not (js2-same-line (point-min)))
                 (not (looking-at "{"))
                 (js2-re-search-backward "[[:graph:]]" nil t)
                 (not (looking-at "[{([]"))
                 (progn
                   (forward-char)
                   (when (= (char-before) ?\))
                     ;; scan-sexps sometimes throws an error
                     (ignore-errors (backward-sexp))
                     (skip-chars-backward " \t" (point-at-bol)))
                   (let ((pt (point)))
                     (back-to-indentation)
                     (when (looking-at "}[ \t]*")
                       (goto-char (match-end 0)))
                     (and (looking-at js2-possibly-braceless-keywords-re)
                          (= (match-end 0) pt)
                          (not (js2-end-of-do-while-loop-p))))))
        (+ (current-indentation) js2-basic-offset)))))

(defun js2-indent-in-array-comp (parse-status)
  "Return non-nil if we think we're in an array comprehension.
In particular, return the buffer position of the first `for' kwd."
  (let ((bracket (nth 1 parse-status))
        (end (point)))
    (when bracket
      (save-excursion
        (goto-char bracket)
        (when (looking-at "\\[")
          (forward-char 1)
          (js2-forward-sws)
          (if (looking-at "[[{]")
              (let (forward-sexp-function) ; use Lisp version
                (forward-sexp)             ; skip destructuring form
                (js2-forward-sws)
                (if (and (/= (char-after) ?,) ; regular array
                         (looking-at "for"))
                    (match-beginning 0)))
            ;; to skip arbitrary expressions we need the parser,
            ;; so we'll just guess at it.
            (if (and (> end (point)) ; not empty literal
                     (re-search-forward "[^,]]* \\(for\\) " end t)
                     ;; not inside comment or string literal
                     (let ((state (parse-partial-sexp bracket (point))))
                       (not (or (nth 3 state) (nth 4 state)))))
                (match-beginning 1))))))))

(defun js2-array-comp-indentation (parse-status for-kwd)
  (if (js2-same-line for-kwd)
      ;; first continuation line
      (save-excursion
        (goto-char (nth 1 parse-status))
        (forward-char 1)
        (skip-chars-forward " \t")
        (current-column))
    (save-excursion
      (goto-char for-kwd)
      (current-column))))

(defun js2-proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (let* ((ctrl-stmt-indent (js2-ctrl-statement-indentation))
           (at-closing-bracket (looking-at "[]})]"))
           (same-indent-p (or at-closing-bracket
                              (looking-at "\\<case\\>[^:]")
                              (and (looking-at "\\<default:")
                                   (save-excursion
                                     (js2-backward-sws)
                                     (not (memq (char-before) '(?, ?{)))))))
           (continued-expr-p (js2-continued-expression-p))
           (declaration-indent (and js2-pretty-multiline-declarations
                                    (js2-multiline-decl-indentation)))
           (bracket (nth 1 parse-status))
           beg indent)
      (cond
       ;; indent array comprehension continuation lines specially
       ((and bracket
             (>= js2-language-version 170)
             (not (js2-same-line bracket))
             (setq beg (js2-indent-in-array-comp parse-status))
             (>= (point) (save-excursion
                           (goto-char beg)
                           (point-at-bol)))) ; at or after first loop?
        (js2-array-comp-indentation parse-status beg))

       (ctrl-stmt-indent)

       ((and declaration-indent continued-expr-p)
        (+ declaration-indent js2-basic-offset))

       (declaration-indent)

       (bracket
        (goto-char bracket)
        (cond
         ((looking-at "[({[][ \t]*\\(/[/*]\\|$\\)")
          (when (save-excursion (skip-chars-backward " \t)")
                                (looking-at ")"))
            (backward-list))
          (back-to-indentation)
          (and (eq js2-pretty-multiline-declarations 'all)
               (looking-at js2-declaration-keyword-re)
               (goto-char (1+ (match-end 0))))
          (setq indent
                (cond (same-indent-p
                       (current-column))
                      (continued-expr-p
                       (+ (current-column) (* 2 js2-basic-offset)))
                      (t
                       (+ (current-column) js2-basic-offset))))
          (if (and js2-indent-switch-body
                   (not at-closing-bracket)
                   (looking-at "\\_<switch\\_>"))
              (+ indent js2-basic-offset)
            indent))
         (t
          (unless same-indent-p
            (forward-char)
            (skip-chars-forward " \t"))
          (current-column))))

       (continued-expr-p js2-basic-offset)

       (t 0)))))

(defun js2-lineup-comment (parse-status)
  "Indent a multi-line block comment continuation line."
  (let* ((beg (nth 8 parse-status))
         (first-line (js2-same-line beg))
         (offset (save-excursion
                   (goto-char beg)
                   (if (looking-at "/\\*")
                       (+ 1 (current-column))
                     0))))
    (unless first-line
      (indent-line-to offset))))

(defun js2-backward-sws ()
  "Move backward through whitespace and comments."
  (interactive)
  (while (forward-comment -1)))

(defun js2-forward-sws ()
  "Move forward through whitespace and comments."
  (interactive)
  (while (forward-comment 1)))

(defun js2-current-indent (&optional pos)
  "Return column of indentation on current line.
If POS is non-nil, go to that point and return indentation for that line."
  (save-excursion
    (if pos
        (goto-char pos))
    (back-to-indentation)
    (current-column)))

(defun js2-arglist-close ()
  "Return non-nil if we're on a line beginning with a close-paren/brace."
  (save-excursion
    (goto-char (point-at-bol))
    (js2-forward-sws)
    (looking-at "[])}]")))

(defun js2-indent-looks-like-label-p ()
  (goto-char (point-at-bol))
  (js2-forward-sws)
  (looking-at (concat js2-mode-identifier-re ":")))

(defun js2-indent-in-objlit-p (parse-status)
  "Return non-nil if this looks like an object-literal entry."
  (let ((start (nth 1 parse-status)))
    (and
     start
     (save-excursion
       (and (zerop (forward-line -1))
            (not (< (point) start))     ; crossed a {} boundary
            (js2-indent-looks-like-label-p)))
     (save-excursion
       (js2-indent-looks-like-label-p)))))

;; If prev line looks like foobar({ then we're passing an object
;; literal to a function call, and people pretty much always want to
;; de-dent back to the previous line, so move the 'basic-offset'
;; position to the front.
(defun js2-indent-objlit-arg-p (parse-status)
  (save-excursion
    (back-to-indentation)
    (js2-backward-sws)
    (and (eq (1- (point)) (nth 1 parse-status))
         (eq (char-before) ?{)
         (progn
           (forward-char -1)
           (skip-chars-backward " \t")
           (eq (char-before) ?\()))))

(defun js2-indent-case-block-p ()
  (save-excursion
    (back-to-indentation)
    (js2-backward-sws)
    (goto-char (point-at-bol))
    (skip-chars-forward " \t")
    (looking-at "case\\s-.+:")))

(defun js2-bounce-indent (normal-col parse-status &optional backwards)
  "Cycle among alternate computed indentation positions.
PARSE-STATUS is the result of `parse-partial-sexp' from the beginning
of the buffer to the current point.  NORMAL-COL is the indentation
column computed by the heuristic guesser based on current paren,
bracket, brace and statement nesting.  If BACKWARDS, cycle positions
in reverse."
  (let ((cur-indent (js2-current-indent))
        (old-buffer-undo-list buffer-undo-list)
        ;; Emacs 21 only has `count-lines', not `line-number-at-pos'
        (current-line (save-excursion
                        (forward-line 0)  ; move to bol
                        (1+ (count-lines (point-min) (point)))))
        positions pos main-pos anchor arglist-cont same-indent
        basic-offset computed-pos)
    ;; temporarily don't record undo info, if user requested this
    (when js2-mode-indent-inhibit-undo
      (setq buffer-undo-list t))
    (unwind-protect
        (progn
          ;; First likely point:  indent from beginning of previous code line
          (push (setq basic-offset
                      (+ (save-excursion
                           (back-to-indentation)
                           (js2-backward-sws)
                           (back-to-indentation)
                           (current-column))
                         js2-basic-offset))
                positions)

          ;; (First + epsilon) likely point:  indent 2x from beginning of
          ;; previous code line.  Google does it this way.
          (push (setq basic-offset
                      (+ (save-excursion
                           (back-to-indentation)
                           (js2-backward-sws)
                           (back-to-indentation)
                           (current-column))
                         (* 2 js2-basic-offset)))
                positions)

          ;; Second likely point:  indent from assign-expr RHS.  This
          ;; is just a crude guess based on finding " = " on the previous
          ;; line containing actual code.
          (setq pos (save-excursion
                      (forward-line -1)
                      (goto-char (point-at-bol))
                      (when (re-search-forward "\\s-+\\(=\\)\\s-+"
                                               (point-at-eol) t)
                        (goto-char (match-end 1))
                        (skip-chars-forward " \t\r\n")
                        (current-column))))
          (when pos
            (incf pos js2-basic-offset)
            (push pos positions))

          ;; Third likely point:  same indent as previous line of code.
          ;; Make it the first likely point if we're not on an
          ;; arglist-close line and previous line ends in a comma, or
          ;; both this line and prev line look like object-literal
          ;; elements.
          (setq pos (save-excursion
                      (goto-char (point-at-bol))
                      (js2-backward-sws)
                      (back-to-indentation)
                      (prog1
                          (current-column)
                        ;; while we're here, look for trailing comma
                        (if (save-excursion
                              (goto-char (point-at-eol))
                              (js2-backward-sws)
                              (eq (char-before) ?,))
                            (setq arglist-cont (1- (point)))))))
          (when pos
            (if (and (or arglist-cont
                         (js2-indent-in-objlit-p parse-status))
                     (not (js2-arglist-close)))
                (setq same-indent pos))
            (push pos positions))

          ;; Fourth likely point:  first preceding code with less indentation.
          ;; than the immediately preceding code line.
          (setq pos (save-excursion
                      (back-to-indentation)
                      (js2-backward-sws)
                      (back-to-indentation)
                      (setq anchor (current-column))
                      (while (and (zerop (forward-line -1))
                                  (>= (progn
                                        (back-to-indentation)
                                        (current-column))
                                      anchor)))
                      (setq pos (current-column))))
          (push pos positions)

          ;; nesting-heuristic position, main by default
          (push (setq main-pos normal-col) positions)

          ;; delete duplicates and sort positions list
          (setq positions (sort (delete-dups positions) '<))

          ;; comma-list continuation lines:  prev line indent takes precedence
          (if same-indent
              (setq main-pos same-indent))

          ;; common special cases where we want to indent in from previous line
          (if (or (js2-indent-case-block-p)
                  (js2-indent-objlit-arg-p parse-status))
              (setq main-pos basic-offset))

          ;; if bouncing backwards, reverse positions list
          (if backwards
              (setq positions (reverse positions)))

          ;; record whether we're already sitting on one of the alternatives
          (setq pos (member cur-indent positions))

          (cond
           ;; case 0:  we're one one of the alternatives and this is the
           ;; first time they've pressed TAB on this line (best-guess).
           ((and js2-mode-indent-ignore-first-tab
                 pos
                 ;; first time pressing TAB on this line?
                 (not (eq js2-mode-last-indented-line current-line)))
            ;; do nothing
            (setq computed-pos nil))
           ;; case 1:  only one computed position => use it
           ((null (cdr positions))
            (setq computed-pos 0))
           ;; case 2:  not on any of the computed spots => use main spot
           ((not pos)
            (setq computed-pos (js2-position main-pos positions)))
           ;; case 3:  on last position:  cycle to first position
           ((null (cdr pos))
            (setq computed-pos 0))
           ;; case 4:  on intermediate position:  cycle to next position
           (t
            (setq computed-pos (js2-position (second pos) positions))))

          ;; see if any hooks want to indent; otherwise we do it
          (loop with result = nil
                for hook in js2-indent-hook
                while (null result)
                do
                (setq result (funcall hook positions computed-pos))
                finally do
                (unless (or result (null computed-pos))
                  (indent-line-to (nth computed-pos positions)))))

      ;; finally
      (if js2-mode-indent-inhibit-undo
          (setq buffer-undo-list old-buffer-undo-list))
      ;; see commentary for `js2-mode-last-indented-line'
      (setq js2-mode-last-indented-line current-line))))

(defun js2-indent-bounce-backwards ()
  "Calls `js2-indent-line'.  When `js2-bounce-indent-p',
cycles between the computed indentation positions in reverse order."
  (interactive)
  (js2-indent-line t))

(defun js2-1-line-comment-continuation-p ()
  "Return t if we're in a 1-line comment continuation.
If so, we don't ever want to use bounce-indent."
  (save-excursion
    (and (progn
           (forward-line 0)
           (looking-at "\\s-*//"))
         (progn
           (forward-line -1)
           (forward-line 0)
           (when (looking-at "\\s-*$")
             (js2-backward-sws)
             (forward-line 0))
           (looking-at "\\s-*//")))))

(defun js2-indent-line (&optional bounce-backwards)
  "Indent the current line as JavaScript source text."
  (interactive)
  (let (parse-status offset indent-col
        ;; Don't whine about errors/warnings when we're indenting.
        ;; This has to be set before calling parse-partial-sexp below.
        (inhibit-point-motion-hooks t))
    (setq parse-status (save-excursion
                         (syntax-ppss (point-at-bol)))
          offset (- (point) (save-excursion
                              (back-to-indentation)
                              (point))))
    (js2-with-underscore-as-word-syntax
     (if (nth 4 parse-status)
         (js2-lineup-comment parse-status)
       (setq indent-col (js2-proper-indentation parse-status))
       ;; See comments below about `js2-mode-last-indented-line'.
       (cond
        ;; bounce-indenting is disabled during electric-key indent.
        ;; It doesn't work well on first line of buffer.
        ((and js2-bounce-indent-p
              (not (js2-same-line (point-min)))
              (not (js2-1-line-comment-continuation-p)))
         (js2-bounce-indent indent-col parse-status bounce-backwards))
        ;; just indent to the guesser's likely spot
        (t (indent-line-to indent-col))))
     (when (plusp offset)
       (forward-char offset)))))

(defun js2-indent-region (start end)
  "Indent the region, but don't use bounce indenting."
  (let ((js2-bounce-indent-p nil)
        (indent-region-function nil)
        (after-change-functions (remq 'js2-mode-edit
                                      after-change-functions)))
    (indent-region start end nil) ; nil for byte-compiler
    (js2-mode-edit start end (- end start))))

(provide 'js2-identation)
