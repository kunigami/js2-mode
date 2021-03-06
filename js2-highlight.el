(defun js2-set-face (beg end face &optional record)
  "Fontify a region.  If RECORD is non-nil, record for later."
  (when (plusp js2-highlight-level)
    (setq beg (min (point-max) beg)
          beg (max (point-min) beg)
          end (min (point-max) end)
          end (max (point-min) end))
    (if record
        (push (list beg end face) js2-mode-fontifications)
      (put-text-property beg end 'font-lock-face face))))

(defsubst js2-clear-face (beg end)
  (remove-text-properties beg end '(font-lock-face nil
                                    help-echo nil
                                    point-entered nil
                                    c-in-sws nil)))

(defconst js2-ecma-global-props
  (concat "^"
          (regexp-opt
           '("Infinity" "NaN" "undefined" "arguments") t)
          "$")
  "Value properties of the Ecma-262 Global Object.
Shown at or above `js2-highlight-level' 2.")

;; might want to add the name "arguments" to this list?
(defconst js2-ecma-object-props
  (concat "^"
          (regexp-opt
           '("prototype" "__proto__" "__parent__") t)
          "$")
  "Value properties of the Ecma-262 Object constructor.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-global-funcs
  (concat
   "^"
   (regexp-opt
    '("decodeURI" "decodeURIComponent" "encodeURI" "encodeURIComponent"
      "eval" "isFinite" "isNaN" "parseFloat" "parseInt") t)
   "$")
  "Function properties of the Ecma-262 Global object.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-number-props
  (concat "^"
          (regexp-opt '("MAX_VALUE" "MIN_VALUE" "NaN"
                        "NEGATIVE_INFINITY"
                        "POSITIVE_INFINITY") t)
          "$")
  "Properties of the Ecma-262 Number constructor.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-date-props "^\\(parse\\|UTC\\)$"
  "Properties of the Ecma-262 Date constructor.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-math-props
  (concat "^"
          (regexp-opt
           '("E" "LN10" "LN2" "LOG2E" "LOG10E" "PI" "SQRT1_2" "SQRT2")
           t)
          "$")
  "Properties of the Ecma-262 Math object.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-math-funcs
  (concat "^"
          (regexp-opt
           '("abs" "acos" "asin" "atan" "atan2" "ceil" "cos" "exp" "floor"
             "log" "max" "min" "pow" "random" "round" "sin" "sqrt" "tan") t)
          "$")
  "Function properties of the Ecma-262 Math object.
Shown at or above `js2-highlight-level' 2.")

(defconst js2-ecma-function-props
  (concat
   "^"
   (regexp-opt
    '(;; properties of the Object prototype object
      "hasOwnProperty" "isPrototypeOf" "propertyIsEnumerable"
      "toLocaleString" "toString" "valueOf"
      ;; properties of the Function prototype object
      "apply" "call"
      ;; properties of the Array prototype object
      "concat" "join" "pop" "push" "reverse" "shift" "slice" "sort"
      "splice" "unshift"
      ;; properties of the String prototype object
      "charAt" "charCodeAt" "fromCharCode" "indexOf" "lastIndexOf"
      "localeCompare" "match" "replace" "search" "split" "substring"
      "toLocaleLowerCase" "toLocaleUpperCase" "toLowerCase"
      "toUpperCase"
      ;; properties of the Number prototype object
      "toExponential" "toFixed" "toPrecision"
      ;; properties of the Date prototype object
      "getDate" "getDay" "getFullYear" "getHours" "getMilliseconds"
      "getMinutes" "getMonth" "getSeconds" "getTime"
      "getTimezoneOffset" "getUTCDate" "getUTCDay" "getUTCFullYear"
      "getUTCHours" "getUTCMilliseconds" "getUTCMinutes" "getUTCMonth"
      "getUTCSeconds" "setDate" "setFullYear" "setHours"
      "setMilliseconds" "setMinutes" "setMonth" "setSeconds" "setTime"
      "setUTCDate" "setUTCFullYear" "setUTCHours" "setUTCMilliseconds"
      "setUTCMinutes" "setUTCMonth" "setUTCSeconds" "toDateString"
      "toLocaleDateString" "toLocaleString" "toLocaleTimeString"
      "toTimeString" "toUTCString"
      ;; properties of the RegExp prototype object
      "exec" "test"
      ;; properties of the JSON prototype object
      "parse" "stringify"
      ;; SpiderMonkey/Rhino extensions, versions 1.5+
      "toSource" "__defineGetter__" "__defineSetter__"
      "__lookupGetter__" "__lookupSetter__" "__noSuchMethod__"
      "every" "filter" "forEach" "lastIndexOf" "map" "some")
    t)
   "$")
  "Built-in functions defined by Ecma-262 and SpiderMonkey extensions.
Shown at or above `js2-highlight-level' 3.")

(defun js2-parse-highlight-prop-get (parent target prop call-p)
  (let ((target-name (and target
                          (js2-name-node-p target)
                          (js2-name-node-name target)))
        (prop-name (if prop (js2-name-node-name prop)))
        (level2 (>= js2-highlight-level 2))
        (level3 (>= js2-highlight-level 3)))
    (when level2
      (let ((face
             (if call-p
                 (cond
                  ((and target prop)
                   (cond
                    ((and level3 (string-match js2-ecma-function-props prop-name))
                     'font-lock-builtin-face)
                    ((and target-name prop)
                     (cond
                      ((string= target-name "Date")
                       (if (string-match js2-ecma-date-props prop-name)
                           'font-lock-builtin-face))
                      ((string= target-name "Math")
                       (if (string-match js2-ecma-math-funcs prop-name)
                           'font-lock-builtin-face))))))
                  (prop
                   (if (string-match js2-ecma-global-funcs prop-name)
                       'font-lock-builtin-face)))
               (cond
                ((and target prop)
                 (cond
                  ((string= target-name "Number")
                   (if (string-match js2-ecma-number-props prop-name)
                       'font-lock-constant-face))
                  ((string= target-name "Math")
                   (if (string-match js2-ecma-math-props prop-name)
                       'font-lock-constant-face))))
                (prop
                 (if (string-match js2-ecma-object-props prop-name)
                     'font-lock-constant-face))))))
        (when face
          (let ((pos (+ (js2-node-pos parent)  ; absolute
                        (js2-node-pos prop)))) ; relative
            (js2-set-face pos
                          (+ pos (js2-node-len prop))
                          face 'record)))))))

(defun js2-parse-highlight-member-expr-node (node)
  "Perform syntax highlighting of EcmaScript built-in properties.
The variable `js2-highlight-level' governs this highighting."
  (let (face target prop name pos end parent call-p callee)
    (cond
     ;; case 1:  simple name, e.g. foo
     ((js2-name-node-p node)
      (setq name (js2-name-node-name node))
      ;; possible for name to be nil in rare cases - saw it when
      ;; running js2-mode on an elisp buffer.  Might as well try to
      ;; make it so js2-mode never barfs.
      (when name
        (setq face (if (string-match js2-ecma-global-props name)
                       'font-lock-constant-face))
        (when face
          (setq pos (js2-node-pos node)
                end (+ pos (js2-node-len node)))
          (js2-set-face pos end face 'record))))
     ;; case 2:  property access or function call
     ((or (js2-prop-get-node-p node)
          ;; highlight function call if expr is a prop-get node
          ;; or a plain name (i.e. unqualified function call)
          (and (setq call-p (js2-call-node-p node))
               (setq callee (js2-call-node-target node)) ; separate setq!
               (or (js2-prop-get-node-p callee)
                   (js2-name-node-p callee))))
      (setq parent node
            node (if call-p callee node))
      (if (and call-p (js2-name-node-p callee))
          (setq prop callee)
        (setq target (js2-prop-get-node-left node)
              prop (js2-prop-get-node-right node)))
      (cond
       ((js2-name-node-p prop)
        ;; case 2(a&c):  simple or complex target, simple name, e.g. x[y].bar
        (js2-parse-highlight-prop-get parent target prop call-p))
       ((js2-name-node-p target)
        ;; case 2b:  simple target, complex name, e.g. foo.x[y]
        (js2-parse-highlight-prop-get parent target nil call-p)))))))

(defun js2-parse-highlight-member-expr-fn-name (expr)
  "Highlight the `baz' in function foo.bar.baz(args) {...}.
This is experimental Rhino syntax.  EXPR is the foo.bar.baz member expr.
We currently only handle the case where the last component is a prop-get
of a simple name.  Called before EXPR has a parent node."
  (let (pos
        (name (and (js2-prop-get-node-p expr)
                   (js2-prop-get-node-right expr))))
    (when (js2-name-node-p name)
      (js2-set-face (setq pos (+ (js2-node-pos expr)  ; parent is absolute
                                 (js2-node-pos name)))
                    (+ pos (js2-node-len name))
                    'font-lock-function-name-face
                    'record))))

;; source:  http://jsdoc.sourceforge.net/
;; Note - this syntax is for Google's enhanced jsdoc parser that
;; allows type specifications, and needs work before entering the wild.

(defconst js2-jsdoc-param-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@"
          "\\(?:param\\|argument\\)"
          "\\)"
          "\\s-*\\({[^}]+}\\)?"         ; optional type
          "\\s-*\\[?\\([[:alnum:]_$\.]+\\)?\\]?"  ; name
          "\\>")
  "Matches jsdoc tags with optional type and optional param name.")

(defconst js2-jsdoc-typed-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
          (regexp-opt
           '("enum"
             "extends"
             "field"
             "id"
             "implements"
             "lends"
             "mods"
             "requires"
             "return"
             "returns"
             "throw"
             "throws"))
          "\\)\\)\\s-*\\({[^}]+}\\)?")
  "Matches jsdoc tags with optional type.")

(defconst js2-jsdoc-arg-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
          (regexp-opt
           '("alias"
             "augments"
             "borrows"
             "bug"
             "base"
             "config"
             "default"
             "define"
             "exception"
             "function"
             "member"
             "memberOf"
             "name"
             "namespace"
             "property"
             "since"
             "suppress"
             "this"
             "throws"
             "type"
             "version"))
          "\\)\\)\\s-+\\([^ \t]+\\)")
  "Matches jsdoc tags with a single argument.")

(defconst js2-jsdoc-empty-tag-regexp
  (concat "^\\s-*\\*+\\s-*\\(@\\(?:"
          (regexp-opt
           '("addon"
             "author"
             "class"
             "const"
             "constant"
             "constructor"
             "constructs"
             "deprecated"
             "desc"
             "description"
             "event"
             "example"
             "exec"
             "export"
             "fileoverview"
             "final"
             "function"
             "hidden"
             "ignore"
             "implicitCast"
             "inheritDoc"
             "inner"
             "interface"
             "license"
             "noalias"
             "noshadow"
             "notypecheck"
             "override"
             "owner"
             "preserve"
             "preserveTry"
             "private"
             "protected"
             "public"
             "static"
             "supported"
             ))
          "\\)\\)\\s-*")
  "Matches empty jsdoc tags.")

(defconst js2-jsdoc-link-tag-regexp
  "{\\(@\\(?:link\\|code\\)\\)\\s-+\\([^#}\n]+\\)\\(#.+\\)?}"
  "Matches a jsdoc link or code tag.")

(defconst js2-jsdoc-see-tag-regexp
  "^\\s-*\\*+\\s-*\\(@see\\)\\s-+\\([^#}\n]+\\)\\(#.+\\)?"
  "Matches a jsdoc @see tag.")

(defconst js2-jsdoc-html-tag-regexp
  "\\(</?\\)\\([[:alpha:]]+\\)\\s-*\\(/?>\\)"
  "Matches a simple (no attributes) html start- or end-tag.")

(defun js2-jsdoc-highlight-helper ()
  (js2-set-face (match-beginning 1)
                (match-end 1)
                'js2-jsdoc-tag)
  (if (match-beginning 2)
      (if (save-excursion
            (goto-char (match-beginning 2))
            (= (char-after) ?{))
          (js2-set-face (1+ (match-beginning 2))
                        (1- (match-end 2))
                        'js2-jsdoc-type)
        (js2-set-face (match-beginning 2)
                      (match-end 2)
                      'js2-jsdoc-value)))
  (if (match-beginning 3)
      (js2-set-face (match-beginning 3)
                    (match-end 3)
                    'js2-jsdoc-value)))

(defun js2-highlight-jsdoc (ast)
  "Highlight doc comment tags."
  (let ((comments (js2-ast-root-comments ast))
        beg end)
    (save-excursion
      (dolist (node comments)
        (when (eq (js2-comment-node-format node) 'jsdoc)
          (setq beg (js2-node-abs-pos node)
                end (+ beg (js2-node-len node)))
          (save-restriction
            (narrow-to-region beg end)
            (dolist (re (list js2-jsdoc-param-tag-regexp
                              js2-jsdoc-typed-tag-regexp
                              js2-jsdoc-arg-tag-regexp
                              js2-jsdoc-link-tag-regexp
                              js2-jsdoc-see-tag-regexp
                              js2-jsdoc-empty-tag-regexp))
              (goto-char beg)
              (while (re-search-forward re nil t)
                (js2-jsdoc-highlight-helper)))
            ;; simple highlighting for html tags
            (goto-char beg)
            (while (re-search-forward js2-jsdoc-html-tag-regexp nil t)
              (js2-set-face (match-beginning 1)
                            (match-end 1)
                            'js2-jsdoc-html-tag-delimiter)
              (js2-set-face (match-beginning 2)
                            (match-end 2)
                            'js2-jsdoc-html-tag-name)
              (js2-set-face (match-beginning 3)
                            (match-end 3)
                            'js2-jsdoc-html-tag-delimiter))))))))

(defun js2-highlight-assign-targets (_node left right)
  "Highlight function properties and external variables."
  (let (leftpos name)
    ;; highlight vars and props assigned function values
    (when (js2-function-node-p right)
      (cond
       ;; var foo = function() {...}
       ((js2-name-node-p left)
        (setq name left))
       ;; foo.bar.baz = function() {...}
       ((and (js2-prop-get-node-p left)
             (js2-name-node-p (js2-prop-get-node-right left)))
        (setq name (js2-prop-get-node-right left))))
      (when name
        (js2-set-face (setq leftpos (js2-node-abs-pos name))
                      (+ leftpos (js2-node-len name))
                      'font-lock-function-name-face
                      'record)))))

(defun js2-record-name-node (node)
  "Saves NODE to `js2-recorded-identifiers' to check for undeclared variables
later. NODE must be a name node."
  (let ((leftpos (js2-node-abs-pos node)))
    (push (list node js2-current-scope
                leftpos
                (+ leftpos (js2-node-len node)))
          js2-recorded-identifiers)))

(defun js2-highlight-undeclared-vars ()
  "After entire parse is finished, look for undeclared variable references.
We have to wait until entire buffer is parsed, since JavaScript permits var
decls to occur after they're used.

If any undeclared var name is in `js2-externs' or `js2-additional-externs',
it is considered declared."
  (let (name)
    (dolist (entry js2-recorded-identifiers)
      (destructuring-bind (name-node scope pos end) entry
        (setq name (js2-name-node-name name-node))
        (unless (or (member name js2-global-externs)
                    (member name js2-default-externs)
                    (member name js2-additional-externs)
                    (js2-get-defining-scope scope name))
          (js2-report-warning "msg.undeclared.variable" name pos (- end pos)
                              'js2-external-variable))))
    (setq js2-recorded-identifiers nil)))

(defun js2-set-default-externs ()
  "Set the value of `js2-default-externs' based on the various
`js2-include-?-externs' variables."
  (setq js2-default-externs
        (append js2-ecma-262-externs
                (if js2-include-browser-externs js2-browser-externs)
                (if js2-include-rhino-externs js2-rhino-externs)
                (if js2-include-node-externs js2-node-externs)
                (if (or js2-include-browser-externs js2-include-node-externs)
                    js2-typed-array-externs))))

(defun js2-apply-jslint-globals ()
  (setq js2-additional-externs
        (nconc (js2-get-jslint-globals)
               js2-additional-externs)))

(defun js2-get-jslint-globals ()
  (loop for node in (js2-ast-root-comments js2-mode-ast)
        when (and (eq 'block (js2-comment-node-format node))
                  (save-excursion
                    (goto-char (js2-node-abs-pos node))
                    (looking-at "/\\*global ")))
        append (js2-get-jslint-globals-in
                (match-end 0)
                (js2-node-abs-end node))))

(defun js2-get-jslint-globals-in (beg end)
  (let (res)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward js2-mode-identifier-re end t)
        (let ((match (match-string 0)))
          (unless (member match '("true" "false"))
            (push match res)))))
    (nreverse res)))

(provide 'js2-highlight)
