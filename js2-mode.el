;;; js2-mode.el --- Improved JavaScript editing mode

;; Copyright (C) 2009, 2011-2014  Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;;         mooz <stillpedant@gmail.com>
;;         Dmitry Gutov <dgutov@yandex.ru>
;; URL:  https://github.com/mooz/js2-mode/
;;       http://code.google.com/p/js2-mode/
;; Version: 20140115
;; Keywords: languages, javascript
;; Package-Requires: ((emacs "24.1"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This JavaScript editing mode supports:

;;  - strict recognition of the Ecma-262 language standard
;;  - support for most Rhino and SpiderMonkey extensions from 1.5 and up
;;  - parsing support for ECMAScript for XML (E4X, ECMA-357)
;;  - accurate syntax highlighting using a recursive-descent parser
;;  - on-the-fly reporting of syntax errors and strict-mode warnings
;;  - undeclared-variable warnings using a configurable externs framework
;;  - "bouncing" line indentation to choose among alternate indentation points
;;  - smart line-wrapping within comments and strings
;;  - code folding:
;;    - show some or all function bodies as {...}
;;    - show some or all block comments as /*...*/
;;  - context-sensitive menu bar and popup menus
;;  - code browsing using the `imenu' package
;;  - many customization options

;; Installation:
;;
;; To install it as your major mode for JavaScript editing:

;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Alternatively, to install it as a minor mode just for JavaScript linting,
;; you must add it to the appropriate major-mode hook.  Normally this would be:

;;   (add-hook 'js-mode-hook 'js2-minor-mode)

;; You may also want to hook it in for shell scripts running via node.js:

;;   (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; To customize how it works:
;;   M-x customize-group RET js2-mode RET

;; Notes:

;; This mode includes a port of Mozilla Rhino's scanner, parser and
;; symbol table.  Ideally it should stay in sync with Rhino, keeping
;; `js2-mode' current as the EcmaScript language standard evolves.

;; Unlike cc-engine based language modes, js2-mode's line-indentation is not
;; customizable.  It is a surprising amount of work to support customizable
;; indentation.  The current compromise is that the tab key lets you cycle among
;; various likely indentation points, similar to the behavior of python-mode.

;; This mode does not yet work with "multi-mode" modes such as `mmm-mode'
;; and `mumamo', although it could be made to do so with some effort.
;; This means that `js2-mode' is currently only useful for editing JavaScript
;; files, and not for editing JavaScript within <script> tags or templates.

;; The project page on GitHub is used for development and issue tracking.
;; The original homepage at Google Code has outdated information and is mostly
;; unmaintained.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'imenu)
(require 'cc-cmds)  ; for `c-fill-paragraph'

(eval-and-compile
  (require 'cc-mode)     ; (only) for `c-populate-syntax-table'
  (require 'cc-engine))  ; for `c-paragraph-start' et. al.

(defvar electric-layout-rules)

;; Customizable option
(require 'js2-custom)
;; Messages for syntax errors
(require 'js2-messages)
;; Rhino tokens
(require 'js2-tokens)
(require 'js2-utils)
(require 'js2-ast)
(require 'js2-scanner)
(require 'js2-highlight)
(require 'js2-imenu)
(require 'js2-parser)
(require 'js2-identation)
(require 'js2-minor-mode)
(provide 'js2-minor-mode)
(provide 'js2-error)

;;; Externs (variables presumed to be defined by the host system)

(defvar js2-ecma-262-externs
  (mapcar 'symbol-name
          '(Array Boolean Date Error EvalError Function Infinity JSON
          Math NaN Number Object RangeError ReferenceError RegExp
          String SyntaxError TypeError URIError arguments
          decodeURI decodeURIComponent encodeURI
          encodeURIComponent escape eval isFinite isNaN
          parseFloat parseInt undefined unescape))
"Ecma-262 externs.  Included in `js2-externs' by default.")

(defvar js2-browser-externs
  (mapcar 'symbol-name
          '(;; DOM level 1
            Attr CDATASection CharacterData Comment DOMException
            DOMImplementation Document DocumentFragment
            DocumentType Element Entity EntityReference
            ExceptionCode NamedNodeMap Node NodeList Notation
            ProcessingInstruction Text

            ;; DOM level 2
            HTMLAnchorElement HTMLAppletElement HTMLAreaElement
            HTMLBRElement HTMLBaseElement HTMLBaseFontElement
            HTMLBodyElement HTMLButtonElement HTMLCollection
            HTMLDListElement HTMLDirectoryElement HTMLDivElement
            HTMLDocument HTMLElement HTMLFieldSetElement
            HTMLFontElement HTMLFormElement HTMLFrameElement
            HTMLFrameSetElement HTMLHRElement HTMLHeadElement
            HTMLHeadingElement HTMLHtmlElement HTMLIFrameElement
            HTMLImageElement HTMLInputElement HTMLIsIndexElement
            HTMLLIElement HTMLLabelElement HTMLLegendElement
            HTMLLinkElement HTMLMapElement HTMLMenuElement
            HTMLMetaElement HTMLModElement HTMLOListElement
            HTMLObjectElement HTMLOptGroupElement
            HTMLOptionElement HTMLOptionsCollection
            HTMLParagraphElement HTMLParamElement HTMLPreElement
            HTMLQuoteElement HTMLScriptElement HTMLSelectElement
            HTMLStyleElement HTMLTableCaptionElement
            HTMLTableCellElement HTMLTableColElement
            HTMLTableElement HTMLTableRowElement
            HTMLTableSectionElement HTMLTextAreaElement
            HTMLTitleElement HTMLUListElement

            ;; DOM level 3
            DOMConfiguration DOMError DOMException
            DOMImplementationList DOMImplementationSource
            DOMLocator DOMStringList NameList TypeInfo
            UserDataHandler

            ;; Window
            window alert confirm document java navigator prompt screen
            self top

            ;; W3C CSS
            CSSCharsetRule CSSFontFace CSSFontFaceRule
            CSSImportRule CSSMediaRule CSSPageRule
            CSSPrimitiveValue CSSProperties CSSRule CSSRuleList
            CSSStyleDeclaration CSSStyleRule CSSStyleSheet
            CSSValue CSSValueList Counter DOMImplementationCSS
            DocumentCSS DocumentStyle ElementCSSInlineStyle
            LinkStyle MediaList RGBColor Rect StyleSheet
            StyleSheetList ViewCSS

            ;; W3C Event
            EventListener EventTarget Event DocumentEvent UIEvent
            MouseEvent MutationEvent KeyboardEvent

            ;; W3C Range
            DocumentRange Range RangeException

            ;; W3C XML
            XPathResult XMLHttpRequest

            ;; console object.  Provided by at least Chrome and Firefox.
            console))
  "Browser externs.
You can cause these to be included or excluded with the custom
variable `js2-include-browser-externs'.")

(defvar js2-rhino-externs
  (mapcar 'symbol-name
          '(Packages importClass importPackage com org java
            ;; Global object (shell) externs.
            defineClass deserialize doctest gc help load
            loadClass print quit readFile readUrl runCommand seal
            serialize spawn sync toint32 version))
  "Mozilla Rhino externs.
Set `js2-include-rhino-externs' to t to include them.")

(defvar js2-node-externs
  (mapcar 'symbol-name
          '(__dirname __filename Buffer clearInterval clearTimeout require
            console exports global module process setInterval setTimeout))
  "Node.js externs.
Set `js2-include-node-externs' to t to include them.")

(defvar js2-typed-array-externs
  (mapcar 'symbol-name
          '(ArrayBuffer Uint8ClampedArray DataView
            Int8Array Uint8Array Int16Array Uint16Array Int32Array Uint32Array
            Float32Array Float64Array))
  "Khronos typed array externs. Available in most modern browsers and
in node.js >= 0.6. If `js2-include-node-externs' or `js2-include-browser-externs'
are enabled, these will also be included.")

;;; Variables

;; scanner variables

;; Rhino accepts any string or stream as input.  Emacs character
;; processing works best in buffers, so we'll assume the input is a
;; buffer.  JavaScript strings can be copied into temp buffers before
;; scanning them.

;; Buffer-local variables yield much cleaner code than using `defstruct'.
;; They're the Emacs equivalent of instance variables, more or less.

(js2-deflocal js2-ts-dirty-line nil
  "Token stream buffer-local variable.
Indicates stuff other than whitespace since start of line.")

(js2-deflocal js2-ts-hit-eof nil
  "Token stream buffer-local variable.")

;; FIXME: Unused.
(js2-deflocal js2-ts-line-start 0
  "Token stream buffer-local variable.")

(js2-deflocal js2-ts-lineno 1
  "Token stream buffer-local variable.")

;; FIXME: Unused.
(js2-deflocal js2-ts-line-end-char -1
  "Token stream buffer-local variable.")

(js2-deflocal js2-ts-cursor 1  ; emacs buffers are 1-indexed
  "Token stream buffer-local variable.
Current scan position.")

;; FIXME: Unused.
(js2-deflocal js2-ts-is-xml-attribute nil
  "Token stream buffer-local variable.")

(js2-deflocal js2-ts-xml-is-tag-content nil
  "Token stream buffer-local variable.")

(js2-deflocal js2-ts-xml-open-tags-count 0
  "Token stream buffer-local variable.")

(js2-deflocal js2-ts-string-buffer nil
  "Token stream buffer-local variable.
List of chars built up while scanning various tokens.")

(defstruct (js2-token
            (:constructor nil)
            (:constructor make-js2-token (beg)))
  "Value returned from the token stream."
  (type js2-EOF)
  (beg 1)
  (end -1)
  (string "")
  number
  regexp-flags
  comment-type
  follows-eol-p)

(defstruct (js2-ts-state
            (:constructor make-js2-ts-state (&key (lineno js2-ts-lineno)
                                                  (cursor js2-ts-cursor)
                                                  (tokens (copy-sequence js2-ti-tokens))
                                                  (tokens-cursor js2-ti-tokens-cursor)
                                                  (lookahead js2-ti-lookahead))))
  lineno
  cursor
  tokens
  tokens-cursor
  lookahead)

;;; Parser variables

(js2-deflocal js2-parsed-errors nil
  "List of errors produced during scanning/parsing.")

(js2-deflocal js2-parsed-warnings nil
  "List of warnings produced during scanning/parsing.")

(js2-deflocal js2-recover-from-parse-errors t
  "Non-nil to continue parsing after a syntax error.

In recovery mode, the AST will be built in full, and any error
nodes will be flagged with appropriate error information.  If
this flag is nil, a syntax error will result in an error being
signaled.

The variable is automatically buffer-local, because different
modes that use the parser will need different settings.")

(js2-deflocal js2-parse-hook nil
  "List of callbacks for receiving parsing progress.")

(defvar js2-parse-finished-hook nil
  "List of callbacks to notify when parsing finishes.
Not called if parsing was interrupted.")

(js2-deflocal js2-is-eval-code nil
  "True if we're evaluating code in a string.
If non-nil, the tokenizer will record the token text, and the AST nodes
will record their source text.  Off by default for IDE modes, since the
text is available in the buffer.")

(defvar js2-parse-ide-mode t
  "Non-nil if the parser is being used for `js2-mode'.
If non-nil, the parser will set text properties for fontification
and the syntax table.  The value should be nil when using the
parser as a frontend to an interpreter or byte compiler.")

;;; Parser instance variables (buffer-local vars for js2-parse)

(defconst js2-ti-after-eol (lsh 1 16)
  "Flag:  first token of the source line.")

;; Inline Rhino's CompilerEnvirons vars as buffer-locals.

(js2-deflocal js2-compiler-generate-debug-info t)
(js2-deflocal js2-compiler-use-dynamic-scope nil)
(js2-deflocal js2-compiler-reserved-keywords-as-identifier nil)
(js2-deflocal js2-compiler-xml-available t)
(js2-deflocal js2-compiler-optimization-level 0)
(js2-deflocal js2-compiler-generating-source t)
(js2-deflocal js2-compiler-strict-mode nil)
(js2-deflocal js2-compiler-report-warning-as-error nil)
(js2-deflocal js2-compiler-generate-observer-count nil)
(js2-deflocal js2-compiler-activation-names nil)

;; SKIP:  sourceURI

;; There's a compileFunction method in Context.java - may need it.
(js2-deflocal js2-called-by-compile-function nil
  "True if `js2-parse' was called by `js2-compile-function'.
Will only be used when we finish implementing the interpreter.")

;; SKIP:  ts  (we just call `js2-init-scanner' and use its vars)

;; SKIP:  node factory - we're going to just call functions directly,
;; and eventually go to a unified AST format.

(js2-deflocal js2-nesting-of-function 0)

(js2-deflocal js2-recorded-identifiers nil
  "Tracks identifiers found during parsing.")

(js2-deflocal js2-is-in-destructuring nil
  "True while parsing destructuring expression.")


;; SKIP:  decompiler
;; SKIP:  encoded-source

;;; The following variables are per-function and should be saved/restored
;;; during function parsing...

(js2-deflocal js2-current-script-or-fn nil)
(js2-deflocal js2-current-scope nil)
(js2-deflocal js2-nesting-of-with 0)
(js2-deflocal js2-label-set nil
  "An alist mapping label names to nodes.")

(js2-deflocal js2-loop-set nil)
(js2-deflocal js2-loop-and-switch-set nil)
(js2-deflocal js2-has-return-value nil)
(js2-deflocal js2-end-flags 0)

;;; ...end of per function variables

;; These flags enumerate the possible ways a statement/function can
;; terminate. These flags are used by endCheck() and by the Parser to
;; detect inconsistent return usage.
;;
;; END_UNREACHED is reserved for code paths that are assumed to always be
;; able to execute (example: throw, continue)
;;
;; END_DROPS_OFF indicates if the statement can transfer control to the
;; next one. Statement such as return dont. A compound statement may have
;; some branch that drops off control to the next statement.
;;
;; END_RETURNS indicates that the statement can return (without arguments)
;; END_RETURNS_VALUE indicates that the statement can return a value.
;;
;; A compound statement such as
;; if (condition) {
;;   return value;
;; }
;; Will be detected as (END_DROPS_OFF | END_RETURN_VALUE) by endCheck()

(defconst js2-end-unreached     #x0)
(defconst js2-end-drops-off     #x1)
(defconst js2-end-returns       #x2)
(defconst js2-end-returns-value #x4)

;; Rhino awkwardly passes a statementLabel parameter to the
;; statementHelper() function, the main statement parser, which
;; is then used by quite a few of the sub-parsers.  We just make
;; it a buffer-local variable and make sure it's cleaned up properly.
(js2-deflocal js2-labeled-stmt nil)  ; type `js2-labeled-stmt-node'

;; Similarly, Rhino passes an inForInit boolean through about half
;; the expression parsers.  We use a dynamically-scoped variable,
;; which makes it easier to funcall the parsers individually without
;; worrying about whether they take the parameter or not.
(js2-deflocal js2-in-for-init nil)
(js2-deflocal js2-temp-name-counter 0)
(js2-deflocal js2-parse-stmt-count 0)

(defsubst js2-get-next-temp-name ()
  (format "$%d" (incf js2-temp-name-counter)))

(defvar js2-parse-interruptable-p t
  "Set this to nil to force parse to continue until finished.
This will mostly be useful for interpreters.")

(defvar js2-statements-per-pause 50
  "Pause after this many statements to check for user input.
If user input is pending, stop the parse and discard the tree.
This makes for a smoother user experience for large files.
You may have to wait a second or two before the highlighting
and error-reporting appear, but you can always type ahead if
you wish.  This appears to be more or less how Eclipse, IntelliJ
and other editors work.")

(js2-deflocal js2-record-comments t
  "Instructs the scanner to record comments in `js2-scanned-comments'.")

(js2-deflocal js2-scanned-comments nil
  "List of all comments from the current parse.")

(defface js2-warning
  `((((class color) (background light))
     (:underline  "orange"))
    (((class color) (background dark))
     (:underline "orange"))
    (t (:underline t)))
  "Face for JavaScript warnings."
  :group 'js2-mode)

(defface js2-error
  `((((class color) (background light))
     (:foreground "red"))
    (((class color) (background dark))
     (:foreground "red"))
    (t (:foreground "red")))
  "Face for JavaScript errors."
  :group 'js2-mode)

(defface js2-jsdoc-tag
  '((t :foreground "SlateGray"))
  "Face used to highlight @whatever tags in jsdoc comments."
  :group 'js2-mode)

(defface js2-jsdoc-type
  '((t :foreground "SteelBlue"))
  "Face used to highlight {FooBar} types in jsdoc comments."
  :group 'js2-mode)

(defface js2-jsdoc-value
  '((t :foreground "PeachPuff3"))
  "Face used to highlight tag values in jsdoc comments."
  :group 'js2-mode)

(defface js2-function-param
  '((t :foreground "SeaGreen"))
  "Face used to highlight function parameters in javascript."
  :group 'js2-mode)

(defface js2-function-call
  '((t :inherit default))
  "Face used to highlight function name in calls."
  :group 'js2-mode)

(defface js2-instance-member
  '((t :foreground "DarkOrchid"))
  "Face used to highlight instance variables in javascript.
Not currently used."
  :group 'js2-mode)

(defface js2-private-member
  '((t :foreground "PeachPuff3"))
  "Face used to highlight calls to private methods in javascript.
Not currently used."
  :group 'js2-mode)

(defface js2-private-function-call
  '((t :foreground "goldenrod"))
  "Face used to highlight calls to private functions in javascript.
Not currently used."
  :group 'js2-mode)

(defface js2-jsdoc-html-tag-name
  '((((class color) (min-colors 88) (background light))
     (:foreground "rosybrown"))
    (((class color) (min-colors 8) (background dark))
     (:foreground "yellow"))
    (((class color) (min-colors 8) (background light))
     (:foreground "magenta")))
    "Face used to highlight jsdoc html tag names"
  :group 'js2-mode)

(defface js2-jsdoc-html-tag-delimiter
  '((((class color) (min-colors 88) (background light))
     (:foreground "dark khaki"))
    (((class color) (min-colors 8) (background dark))
     (:foreground "green"))
    (((class color) (min-colors 8) (background light))
     (:foreground "green")))
  "Face used to highlight brackets in jsdoc html tags."
  :group 'js2-mode)

(defface js2-external-variable
  '((t :foreground "orange"))
  "Face used to highlight undeclared variable identifiers.")


(defvar js2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'js2-mode-show-node)
    (define-key map (kbd "M-j") #'js2-line-break)
    (define-key map (kbd "C-c C-e") #'js2-mode-hide-element)
    (define-key map (kbd "C-c C-s") #'js2-mode-show-element)
    (define-key map (kbd "C-c C-a") #'js2-mode-show-all)
    (define-key map (kbd "C-c C-f") #'js2-mode-toggle-hide-functions)
    (define-key map (kbd "C-c C-t") #'js2-mode-toggle-hide-comments)
    (define-key map (kbd "C-c C-o") #'js2-mode-toggle-element)
    (define-key map (kbd "C-c C-w") #'js2-mode-toggle-warnings-and-errors)
    (define-key map [down-mouse-3] #'js2-down-mouse-3)
    (when js2-bounce-indent-p
      (define-key map (kbd "<backtab>") #'js2-indent-bounce-backwards))

    (define-key map [menu-bar javascript]
      (cons "JavaScript" (make-sparse-keymap "JavaScript")))

    (define-key map [menu-bar javascript customize-js2-mode]
      '(menu-item "Customize js2-mode" js2-mode-customize
                  :help "Customize the behavior of this mode"))

    (define-key map [menu-bar javascript js2-force-refresh]
      '(menu-item "Force buffer refresh" js2-mode-reset
                  :help "Re-parse the buffer from scratch"))

    (define-key map [menu-bar javascript separator-2]
      '("--"))

    (define-key map [menu-bar javascript next-error]
      '(menu-item "Next warning or error" next-error
                  :enabled (and js2-mode-ast
                                (or (js2-ast-root-errors js2-mode-ast)
                                    (js2-ast-root-warnings js2-mode-ast)))
                  :help "Move to next warning or error"))

    (define-key map [menu-bar javascript display-errors]
      '(menu-item "Show errors and warnings" js2-mode-display-warnings-and-errors
                  :visible (not js2-mode-show-parse-errors)
                  :help "Turn on display of warnings and errors"))

    (define-key map [menu-bar javascript hide-errors]
      '(menu-item "Hide errors and warnings" js2-mode-hide-warnings-and-errors
                  :visible js2-mode-show-parse-errors
                  :help "Turn off display of warnings and errors"))

    (define-key map [menu-bar javascript separator-1]
      '("--"))

    (define-key map [menu-bar javascript js2-toggle-function]
      '(menu-item "Show/collapse element" js2-mode-toggle-element
                  :help "Hide or show function body or comment"))

    (define-key map [menu-bar javascript show-comments]
      '(menu-item "Show block comments" js2-mode-toggle-hide-comments
                  :visible js2-mode-comments-hidden
                  :help "Expand all hidden block comments"))

    (define-key map [menu-bar javascript hide-comments]
      '(menu-item "Hide block comments" js2-mode-toggle-hide-comments
                  :visible (not js2-mode-comments-hidden)
                  :help "Show block comments as /*...*/"))

    (define-key map [menu-bar javascript show-all-functions]
      '(menu-item "Show function bodies" js2-mode-toggle-hide-functions
                  :visible js2-mode-functions-hidden
                  :help "Expand all hidden function bodies"))

    (define-key map [menu-bar javascript hide-all-functions]
      '(menu-item "Hide function bodies" js2-mode-toggle-hide-functions
                  :visible (not js2-mode-functions-hidden)
                  :help "Show {...} for all top-level function bodies"))

    map)
  "Keymap used in `js2-mode' buffers.")

(defconst js2-mode-identifier-re "[[:alpha:]_$][[:alnum:]_$]*")

(defvar js2-mode-//-comment-re "^\\(\\s-*\\)//.+"
  "Matches a //-comment line.  Must be first non-whitespace on line.
First match-group is the leading whitespace.")

(defvar js2-mode-hook nil)

(js2-deflocal js2-mode-ast nil "Private variable.")
(js2-deflocal js2-mode-parse-timer nil "Private variable.")
(js2-deflocal js2-mode-buffer-dirty-p nil "Private variable.")
(js2-deflocal js2-mode-parsing nil "Private variable.")
(js2-deflocal js2-mode-node-overlay nil)

(defvar js2-mode-show-overlay js2-mode-dev-mode-p
  "Debug:  Non-nil to highlight AST nodes on mouse-down.")

(js2-deflocal js2-mode-fontifications nil "Private variable")
(js2-deflocal js2-mode-deferred-properties nil "Private variable")
(js2-deflocal js2-imenu-recorder nil "Private variable")
(js2-deflocal js2-imenu-function-map nil "Private variable")

(defvar js2-paragraph-start
  "\\(@[[:alpha:]]+\\>\\|$\\)")

;; Note that we also set a 'c-in-sws text property in html comments,
;; so that `c-forward-sws' and `c-backward-sws' work properly.
(defvar js2-syntactic-ws-start
  "\\s \\|/[*/]\\|[\n\r]\\|\\\\[\n\r]\\|\\s!\\|<!--\\|^\\s-*-->")

(defvar js2-syntactic-ws-end
  "\\s \\|[\n\r/]\\|\\s!")

(defvar js2-syntactic-eol
  (concat "\\s *\\(/\\*[^*\n\r]*"
          "\\(\\*+[^*\n\r/][^*\n\r]*\\)*"
          "\\*+/\\s *\\)*"
          "\\(//\\|/\\*[^*\n\r]*"
          "\\(\\*+[^*\n\r/][^*\n\r]*\\)*$"
          "\\|\\\\$\\|$\\)")
  "Copied from `java-mode'.  Needed for some cc-engine functions.")

(defvar js2-comment-prefix-regexp
  "//+\\|\\**")

(defvar js2-comment-start-skip
  "\\(//+\\|/\\*+\\)\\s *")

(defvar js2-mode-verbose-parse-p js2-mode-dev-mode-p
  "Non-nil to emit status messages during parsing.")

(defvar js2-mode-functions-hidden nil "Private variable.")
(defvar js2-mode-comments-hidden nil "Private variable.")

(defvar js2-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    table)
  "Syntax table used in `js2-mode' buffers.")

(defvar js2-mode-abbrev-table nil
  "Abbrev table in use in `js2-mode' buffers.")
(define-abbrev-table 'js2-mode-abbrev-table ())

(defvar js2-mode-pending-parse-callbacks nil
  "List of functions waiting to be notified that parse is finished.")

(defvar js2-mode-last-indented-line -1)

;;; Localizable error and warning messages

;; Messages are copied from Rhino's Messages.properties.
;; Many of the Java-specific messages have been elided.
;; Add any js2-specific ones at the end, so we can keep
;; this file synced with changes to Rhino's.
;;
;; See js2-mode

(defun js2-get-msg (msg-key)
  "Look up a localized message.
MSG-KEY is a list of (MSG ARGS).  If the message takes parameters,
the correct number of ARGS must be provided."
  (let* ((key (if (listp msg-key) (car msg-key) msg-key))
         (args (if (listp msg-key) (cdr msg-key)))
         (msg (gethash key js2-message-table)))
    (if msg
        (apply #'format msg args)
      key)))  ; default to showing the key

;;; Tokens Buffer

(defconst js2-ti-max-lookahead 2)
(defconst js2-ti-ntokens (1+ js2-ti-max-lookahead))

;; Have to call `js2-init-scanner' to initialize the values.
(js2-deflocal js2-ti-tokens nil)
(js2-deflocal js2-ti-tokens-cursor nil)
(js2-deflocal js2-ti-lookahead nil)

(defun js2-new-token (offset)
  (let ((token (make-js2-token (+ offset js2-ts-cursor))))
    (setq js2-ti-tokens-cursor (mod (1+ js2-ti-tokens-cursor) js2-ti-ntokens))
    (aset js2-ti-tokens js2-ti-tokens-cursor token)
    token))

(defsubst js2-current-token ()
  (aref js2-ti-tokens js2-ti-tokens-cursor))

(defsubst js2-current-token-string ()
  (js2-token-string (js2-current-token)))

(defsubst js2-current-token-type ()
  (js2-token-type (js2-current-token)))

(defsubst js2-current-token-beg ()
  (js2-token-beg (js2-current-token)))

(defsubst js2-current-token-end ()
  (js2-token-end (js2-current-token)))

(defun js2-current-token-len ()
  (let ((token (js2-current-token)))
    (- (js2-token-end token)
       (js2-token-beg token))))

(defun js2-ts-seek (state)
  (setq js2-ts-lineno (js2-ts-state-lineno state)
        js2-ts-cursor (js2-ts-state-cursor state)
        js2-ti-tokens (js2-ts-state-tokens state)
        js2-ti-tokens-cursor (js2-ts-state-tokens-cursor state)
        js2-ti-lookahead (js2-ts-state-lookahead state)))

;;; Utilities
;;; see js2-utils

;;; AST struct and function definitions
;;; see js2-ast

;;; Scanner -- a port of Mozilla Rhino's lexer.
;; Corresponds to Rhino files Token.java and TokenStream.java.
;; see js2-scanner

;;; Highlighting
;; see js2-highlight

;;; IMenu support
;; see js2-imenu

;;; Parser
;;; See js2-parser

;;; Indentation support
;;; See js2-identation

;;; Minor mode
;;; See js2-minor-mode

(defvar js2-source-buffer nil "Linked source buffer for diagnostics view")
(make-variable-buffer-local 'js2-source-buffer)

(defun* js2-display-error-list ()
  "Display a navigable buffer listing parse errors/warnings."
  (interactive)
  (unless (js2-have-errors-p)
    (message "No errors")
    (return-from js2-display-error-list))
  (labels ((annotate-list
            (lst type)
            "Add diagnostic TYPE and line number to errs list"
            (mapcar (lambda (err)
                      (list err type (line-number-at-pos (nth 1 err))))
                    lst)))
    (let* ((srcbuf (current-buffer))
           (errbuf (get-buffer-create "*js-lint*"))
           (errors (annotate-list
                    (when js2-mode-ast (js2-ast-root-errors js2-mode-ast))
                    'js2-error))  ; must be a valid face name
           (warnings (annotate-list
                      (when js2-mode-ast (js2-ast-root-warnings js2-mode-ast))
                      'js2-warning))  ; must be a valid face name
           (all-errs (sort (append errors warnings)
                           (lambda (e1 e2) (< (cadar e1) (cadar e2))))))
      (with-current-buffer errbuf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (err all-errs)
            (destructuring-bind ((msg-key beg _end &rest) type line) err
              (insert-text-button
               (format "line %d: %s" line (js2-get-msg msg-key))
               'face type
               'follow-link "\C-m"
               'action 'js2-error-buffer-jump
               'js2-msg (js2-get-msg msg-key)
               'js2-pos beg)
              (insert "\n"))))
        (js2-error-buffer-mode)
        (setq js2-source-buffer srcbuf)
        (pop-to-buffer errbuf)
        (goto-char (point-min))
        (unless (eobp)
          (js2-error-buffer-view))))))

(defvar js2-error-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'js2-error-buffer-next)
    (define-key map "p" #'js2-error-buffer-prev)
    (define-key map (kbd "RET") #'js2-error-buffer-jump)
    (define-key map "o" #'js2-error-buffer-view)
    (define-key map "q" #'js2-error-buffer-quit)
    map)
  "Keymap used for js2 diagnostics buffers.")

(defun js2-error-buffer-mode ()
  "Major mode for js2 diagnostics buffers.
Selecting an error will jump it to the corresponding source-buffer error.
\\{js2-error-buffer-mode-map}"
  (interactive)
  (setq major-mode 'js2-error-buffer-mode
        mode-name "JS Lint Diagnostics")
  (use-local-map js2-error-buffer-mode-map)
  (setq truncate-lines t)
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (run-hooks 'js2-error-buffer-mode-hook))

(defun js2-error-buffer-next ()
  "Move to next error and view it."
  (interactive)
  (when (zerop (forward-line 1))
    (js2-error-buffer-view)))

(defun js2-error-buffer-prev ()
  "Move to previous error and view it."
  (interactive)
  (when (zerop (forward-line -1))
    (js2-error-buffer-view)))

(defun js2-error-buffer-quit ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer))

(defun js2-error-buffer-jump (&rest ignored)
  "Jump cursor to current error in source buffer."
  (interactive)
  (when (js2-error-buffer-view)
    (pop-to-buffer js2-source-buffer)))

(defun js2-error-buffer-view ()
  "Scroll source buffer to show error at current line."
  (interactive)
  (cond
   ((not (eq major-mode 'js2-error-buffer-mode))
    (message "Not in a js2 errors buffer"))
   ((not (buffer-live-p js2-source-buffer))
    (message "Source buffer has been killed"))
   ((not (wholenump (get-text-property (point) 'js2-pos)))
    (message "There does not seem to be an error here"))
   (t
    (let ((pos (get-text-property (point) 'js2-pos))
          (msg (get-text-property (point) 'js2-msg)))
      (save-selected-window
        (pop-to-buffer js2-source-buffer)
        (goto-char pos)
        (message msg))))))

;;;###autoload
(define-derived-mode js2-mode prog-mode "Javascript-IDE"
  ;; FIXME: Should derive from js-mode.
  "Major mode for editing JavaScript code."
  ;; Used by comment-region; don't change it.
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) js2-comment-start-skip)
  (set (make-local-variable 'max-lisp-eval-depth)
       (max max-lisp-eval-depth 3000))
  (set (make-local-variable 'indent-line-function) #'js2-indent-line)
  (set (make-local-variable 'indent-region-function) #'js2-indent-region)
  (set (make-local-variable 'fill-paragraph-function) #'c-fill-paragraph)
  (set (make-local-variable 'comment-line-break-function) #'js2-line-break)
  (set (make-local-variable 'beginning-of-defun-function) #'js2-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function) #'js2-end-of-defun)
  ;; We un-confuse `parse-partial-sexp' by setting syntax-table properties
  ;; for characters inside regexp literals.
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  ;; this is necessary to make `show-paren-function' work properly
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; needed for M-x rgrep, among other things
  (put 'js2-mode 'find-tag-default-function #'js2-mode-find-tag)

  (set (make-local-variable 'electric-indent-chars)
       (append "{}()[]:;,*" electric-indent-chars))
  (set (make-local-variable 'electric-layout-rules)
       '((?\; . after) (?\{ . after) (?\} . before)))

  ;; some variables needed by cc-engine for paragraph-fill, etc.
  (setq c-comment-prefix-regexp js2-comment-prefix-regexp
        c-comment-start-regexp "/[*/]\\|\\s|"
        c-line-comment-starter "//"
        c-paragraph-start js2-paragraph-start
        c-paragraph-separate "$"
        c-syntactic-ws-start js2-syntactic-ws-start
        c-syntactic-ws-end js2-syntactic-ws-end
        c-syntactic-eol js2-syntactic-eol)

  (let ((c-buffer-is-cc-mode t))
    ;; Copied from `js-mode'.  Also see Bug#6071.
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (make-local-variable 'adaptive-fill-mode)
    (make-local-variable 'adaptive-fill-regexp)
    (c-setup-paragraph-variables))

  (setq font-lock-defaults '(nil t))

  ;; Experiment:  make reparse-delay longer for longer files.
  (when (plusp js2-dynamic-idle-timer-adjust)
    (setq js2-idle-timer-delay
          (* js2-idle-timer-delay
             (/ (point-max) js2-dynamic-idle-timer-adjust))))

  (add-hook 'change-major-mode-hook #'js2-mode-exit nil t)
  (add-hook 'after-change-functions #'js2-mode-edit nil t)
  (setq imenu-create-index-function #'js2-mode-create-imenu-index)
  (setq next-error-function #'js2-next-error)
  (imenu-add-to-menubar (concat "IM-" mode-name))
  (add-to-invisibility-spec '(js2-outline . t))
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (set (make-local-variable 'forward-sexp-function) #'js2-mode-forward-sexp)

  (setq js2-mode-functions-hidden nil
        js2-mode-comments-hidden nil
        js2-mode-buffer-dirty-p t
        js2-mode-parsing nil)

  (js2-set-default-externs)

  (when js2-include-jslint-globals
    (add-hook 'js2-post-parse-callbacks 'js2-apply-jslint-globals nil t))

  (run-hooks 'js2-init-hook)

  (js2-reparse))

(defun js2-mode-exit ()
  "Exit `js2-mode' and clean up."
  (interactive)
  (when js2-mode-node-overlay
    (delete-overlay js2-mode-node-overlay)
    (setq js2-mode-node-overlay nil))
  (js2-remove-overlays)
  (setq js2-mode-ast nil)
  (remove-hook 'change-major-mode-hook #'js2-mode-exit t)
  (remove-from-invisibility-spec '(js2-outline . t))
  (js2-mode-show-all)
  (with-silent-modifications
    (js2-clear-face (point-min) (point-max))))

(defun js2-mode-reset-timer ()
  "Cancel any existing parse timer and schedule a new one."
  (if js2-mode-parse-timer
      (cancel-timer js2-mode-parse-timer))
  (setq js2-mode-parsing nil)
  (let ((timer (timer-create)))
    (setq js2-mode-parse-timer timer)
    (timer-set-function timer 'js2-mode-idle-reparse (list (current-buffer)))
    (timer-set-idle-time timer js2-idle-timer-delay)
    ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=12326
    (timer-activate-when-idle timer nil)))

(defun js2-mode-idle-reparse (buffer)
  "Run `js2-reparse' if BUFFER is the current buffer, or schedule
it to be reparsed when the buffer is selected."
  (if (eq buffer (current-buffer))
      (js2-reparse)
    ;; reparse when the buffer is selected again
    (with-current-buffer buffer
      (add-hook 'window-configuration-change-hook
                #'js2-mode-idle-reparse-inner
                nil t))))

(defun js2-mode-idle-reparse-inner ()
  (remove-hook 'window-configuration-change-hook
               #'js2-mode-idle-reparse-inner
               t)
  (js2-reparse))

(defun js2-mode-edit (_beg _end _len)
  "Schedule a new parse after buffer is edited.
Buffer edit spans from BEG to END and is of length LEN."
  (setq js2-mode-buffer-dirty-p t)
  (js2-mode-hide-overlay)
  (js2-mode-reset-timer))

(defun js2-minor-mode-edit (_beg _end _len)
  "Callback for buffer edits in `js2-mode'.
Schedules a new parse after buffer is edited.
Buffer edit spans from BEG to END and is of length LEN."
  (setq js2-mode-buffer-dirty-p t)
  (js2-mode-hide-overlay)
  (js2-mode-reset-timer))

(defun js2-reparse (&optional force)
  "Re-parse current buffer after user finishes some data entry.
If we get any user input while parsing, including cursor motion,
we discard the parse and reschedule it.  If FORCE is nil, then the
buffer will only rebuild its `js2-mode-ast' if the buffer is dirty."
  (let (time
        interrupted-p
        (js2-compiler-strict-mode js2-mode-show-strict-warnings))
    (unless js2-mode-parsing
      (setq js2-mode-parsing t)
      (unwind-protect
          (when (or js2-mode-buffer-dirty-p force)
            (js2-remove-overlays)
            (with-silent-modifications
              (setq js2-mode-buffer-dirty-p nil
                    js2-mode-fontifications nil
                    js2-mode-deferred-properties nil)
              (if js2-mode-verbose-parse-p
                  (message "parsing..."))
              (setq time
                    (js2-time
                     (setq interrupted-p
                           (catch 'interrupted
                             (js2-parse)
                             ;; if parsing is interrupted, comments and regex
                             ;; literals stay ignored by `parse-partial-sexp'
                             (remove-text-properties (point-min) (point-max)
                                                     '(syntax-table))
                             (js2-mode-apply-deferred-properties)
                             (js2-mode-remove-suppressed-warnings)
                             (js2-mode-show-warnings)
                             (js2-mode-show-errors)
                             (if (>= js2-highlight-level 1)
                                 (js2-highlight-jsdoc js2-mode-ast))
                             nil))))
              (if interrupted-p
                  (progn
                    ;; unfinished parse => try again
                    (setq js2-mode-buffer-dirty-p t)
                    (js2-mode-reset-timer))
                (if js2-mode-verbose-parse-p
                    (message "Parse time: %s" time)))))
        (setq js2-mode-parsing nil)
        (unless interrupted-p
          (setq js2-mode-parse-timer nil))))))

(defun js2-mode-show-node (event)
  "Debugging aid:  highlight selected AST node on mouse click."
  (interactive "e")
  (mouse-set-point event)
  (setq deactivate-mark t)
  (when js2-mode-show-overlay
    (let ((node (js2-node-at-point))
          beg end)
      (if (null node)
          (message "No node found at location %s" (point))
        (setq beg (js2-node-abs-pos node)
              end (+ beg (js2-node-len node)))
        (if js2-mode-node-overlay
            (move-overlay js2-mode-node-overlay beg end)
          (setq js2-mode-node-overlay (make-overlay beg end))
          (overlay-put js2-mode-node-overlay 'font-lock-face 'highlight))
        (with-silent-modifications
          (put-text-property beg end 'point-left #'js2-mode-hide-overlay))
        (message "%s, parent: %s"
                 (js2-node-short-name node)
                 (if (js2-node-parent node)
                     (js2-node-short-name (js2-node-parent node))
                   "nil"))))))

(defun js2-mode-hide-overlay (&optional _p1 p2)
  "Remove the debugging overlay when the point moves.
P1 and P2 are the old and new values of point, respectively."
  (when js2-mode-node-overlay
    (let ((beg (overlay-start js2-mode-node-overlay))
          (end (overlay-end js2-mode-node-overlay)))
      ;; Sometimes we're called spuriously.
      (unless (and p2
                   (>= p2 beg)
                   (<= p2 end))
        (with-silent-modifications
          (remove-text-properties beg end '(point-left nil)))
        (delete-overlay js2-mode-node-overlay)
        (setq js2-mode-node-overlay nil)))))

(defun js2-mode-reset ()
  "Debugging helper:  reset everything."
  (interactive)
  (js2-mode-exit)
  (js2-mode))

(defun js2-mode-show-warn-or-err (e face)
  "Highlight a warning or error E with FACE.
E is a list of ((MSG-KEY MSG-ARG) BEG LEN OVERRIDE-FACE).
The last element is optional.  When present, use instead of FACE."
  (let* ((key (first e))
         (beg (second e))
         (end (+ beg (third e)))
         ;; Don't inadvertently go out of bounds.
         (beg (max (point-min) (min beg (point-max))))
         (end (max (point-min) (min end (point-max))))
         (js2-highlight-level 3)    ; so js2-set-face is sure to fire
         (ovl (make-overlay beg end)))
    (overlay-put ovl 'font-lock-face (or (fourth e) face))
    (overlay-put ovl 'js2-error t)
    (put-text-property beg end 'help-echo (js2-get-msg key))
    (put-text-property beg end 'point-entered #'js2-echo-error)))

(defun js2-remove-overlays ()
  "Remove overlays from buffer that have a `js2-error' property."
  (let ((beg (point-min))
        (end (point-max)))
    (save-excursion
      (dolist (o (overlays-in beg end))
        (when (overlay-get o 'js2-error)
          (delete-overlay o))))))

(defun js2-error-at-point (&optional pos)
  "Return non-nil if there's an error overlay at POS.
Defaults to point."
  (loop with pos = (or pos (point))
        for o in (overlays-at pos)
        thereis (overlay-get o 'js2-error)))

(defun js2-mode-apply-deferred-properties ()
  "Apply fontifications and other text properties recorded during parsing."
  (when (plusp js2-highlight-level)
    ;; We defer clearing faces as long as possible to eliminate flashing.
    (js2-clear-face (point-min) (point-max))
    ;; Have to reverse the recorded fontifications list so that errors
    ;; and warnings overwrite the normal fontifications.
    (dolist (f (nreverse js2-mode-fontifications))
      (put-text-property (first f) (second f) 'font-lock-face (third f)))
    (setq js2-mode-fontifications nil))
  (dolist (p js2-mode-deferred-properties)
    (apply #'put-text-property p))
  (setq js2-mode-deferred-properties nil))

(defun js2-mode-show-errors ()
  "Highlight syntax errors."
  (when js2-mode-show-parse-errors
    (dolist (e (js2-ast-root-errors js2-mode-ast))
      (js2-mode-show-warn-or-err e 'js2-error))))

(defun js2-mode-remove-suppressed-warnings ()
  "Take suppressed warnings out of the AST warnings list.
This ensures that the counts and `next-error' are correct."
  (setf (js2-ast-root-warnings js2-mode-ast)
        (js2-delete-if
         (lambda (e)
           (let ((key (caar e)))
             (or
              (and (not js2-strict-trailing-comma-warning)
                   (string-match "trailing\\.comma" key))
              (and (not js2-strict-cond-assign-warning)
                   (string= key "msg.equal.as.assign"))
              (and js2-missing-semi-one-line-override
                   (string= key "msg.missing.semi")
                   (let* ((beg (second e))
                          (node (js2-node-at-point beg))
                          (fn (js2-mode-find-parent-fn node))
                          (body (and fn (js2-function-node-body fn)))
                          (lc (and body (js2-node-abs-pos body)))
                          (rc (and lc (+ lc (js2-node-len body)))))
                     (and fn
                          (or (null body)
                              (save-excursion
                                (goto-char beg)
                                (and (js2-same-line lc)
                                     (js2-same-line rc))))))))))
         (js2-ast-root-warnings js2-mode-ast))))

(defun js2-mode-show-warnings ()
  "Highlight strict-mode warnings."
  (when js2-mode-show-strict-warnings
    (dolist (e (js2-ast-root-warnings js2-mode-ast))
      (js2-mode-show-warn-or-err e 'js2-warning))))

(defun js2-echo-error (_old-point new-point)
  "Called by point-motion hooks."
  (let ((msg (get-text-property new-point 'help-echo)))
    (when (and (stringp msg)
               (not (active-minibuffer-window))
               (not (current-message)))
      (message msg))))

(defalias 'js2-echo-help #'js2-echo-error)

(defun js2-line-break (&optional _soft)
  "Break line at point and indent, continuing comment if within one.
If inside a string, and `js2-concat-multiline-strings' is not
nil, turn it into concatenation."
  (interactive)
  (let ((parse-status (syntax-ppss)))
    (cond
     ;; Check if we're inside a string.
     ((nth 3 parse-status)
      (if js2-concat-multiline-strings
          (js2-mode-split-string parse-status)
        (insert "\n")))
     ;; Check if inside a block comment.
     ((nth 4 parse-status)
      (js2-mode-extend-comment (nth 8 parse-status)))
     (t
      (newline-and-indent)))))

(defun js2-mode-split-string (parse-status)
  "Turn a newline in mid-string into a string concatenation.
PARSE-STATUS is as documented in `parse-partial-sexp'."
  (let* ((quote-char (nth 3 parse-status))
         (at-eol (eq js2-concat-multiline-strings 'eol)))
    (insert quote-char)
    (insert (if at-eol " +\n" "\n"))
    (unless at-eol
      (insert "+ "))
    (js2-indent-line)
    (insert quote-char)
    (when (eolp)
      (insert quote-char)
      (backward-char 1))))

(defun js2-mode-extend-comment (start-pos)
  "Indent the line and, when inside a comment block, add comment prefix."
  (let (star single col first-line needs-close)
    (save-excursion
      (back-to-indentation)
      (when (< (point) start-pos)
        (goto-char start-pos))
      (cond
       ((looking-at "\\*[^/]")
        (setq star t
              col (current-column)))
       ((looking-at "/\\*")
        (setq star t
              first-line t
              col (1+ (current-column))))
       ((looking-at "//")
        (setq single t
              col (current-column)))))
    ;; Heuristic for whether we need to close the comment:
    ;; if we've got a parse error here, assume it's an unterminated
    ;; comment.
    (setq needs-close
          (or
           (eq (get-text-property (1- (point)) 'point-entered)
               'js2-echo-error)
           ;; The heuristic above doesn't work well when we're
           ;; creating a comment and there's another one downstream,
           ;; as our parser thinks this one ends at the end of the
           ;; next one.  (You can have a /* inside a js block comment.)
           ;; So just close it if the next non-ws char isn't a *.
           (and first-line
                (eolp)
                (save-excursion
                  (skip-chars-forward " \t\r\n")
                  (not (eq (char-after) ?*))))))
    (delete-horizontal-space)
    (insert "\n")
    (cond
     (star
      (indent-to col)
      (insert "* ")
      (if (and first-line needs-close)
          (save-excursion
            (insert "\n")
            (indent-to col)
            (insert "*/"))))
     ((and single
           (save-excursion
              (and (zerop (forward-line 1))
                   (looking-at "\\s-*//"))))
      (indent-to col)
      (insert "// ")))
    ;; Don't need to extend the comment after all.
    (js2-indent-line)))

(defun js2-beginning-of-line ()
  "Toggle point between bol and first non-whitespace char in line.
Also moves past comment delimiters when inside comments."
  (interactive)
  (let (node)
    (cond
     ((bolp)
      (back-to-indentation))
     ((looking-at "//")
      (skip-chars-forward "/ \t"))
     ((and (eq (char-after) ?*)
           (setq node (js2-comment-at-point))
           (memq (js2-comment-node-format node) '(jsdoc block))
           (save-excursion
             (skip-chars-backward " \t")
             (bolp)))
      (skip-chars-forward "\* \t"))
     (t
      (goto-char (point-at-bol))))))

(defun js2-end-of-line ()
  "Toggle point between eol and last non-whitespace char in line."
  (interactive)
  (if (eolp)
      (skip-chars-backward " \t")
    (goto-char (point-at-eol))))

(defun js2-mode-wait-for-parse (callback)
  "Invoke CALLBACK when parsing is finished.
If parsing is already finished, calls CALLBACK immediately."
  (if (not js2-mode-buffer-dirty-p)
      (funcall callback)
    (push callback js2-mode-pending-parse-callbacks)
    (add-hook 'js2-parse-finished-hook #'js2-mode-parse-finished)))

(defun js2-mode-parse-finished ()
  "Invoke callbacks in `js2-mode-pending-parse-callbacks'."
  ;; We can't let errors propagate up, since it prevents the
  ;; `js2-parse' method from completing normally and returning
  ;; the ast, which makes things mysteriously not work right.
  (unwind-protect
      (dolist (cb js2-mode-pending-parse-callbacks)
        (condition-case err
            (funcall cb)
          (error (message "%s" err))))
    (setq js2-mode-pending-parse-callbacks nil)))

(defun js2-mode-flag-region (from to flag)
  "Hide or show text from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden.
Returns the created overlay if FLAG is non-nil."
  (remove-overlays from to 'invisible 'js2-outline)
  (when flag
    (let ((o (make-overlay from to)))
      (overlay-put o 'invisible 'js2-outline)
      (overlay-put o 'isearch-open-invisible
                   'js2-isearch-open-invisible)
      o)))

;; Function to be set as an outline-isearch-open-invisible' property
;; to the overlay that makes the outline invisible (see
;; `js2-mode-flag-region').
(defun js2-isearch-open-invisible (_overlay)
  ;; We rely on the fact that isearch places point on the matched text.
  (js2-mode-show-element))

(defun js2-mode-invisible-overlay-bounds (&optional pos)
  "Return cons cell of bounds of folding overlay at POS.
Returns nil if not found."
  (let ((overlays (overlays-at (or pos (point))))
        o)
    (while (and overlays
                (not o))
      (if (overlay-get (car overlays) 'invisible)
          (setq o (car overlays))
        (setq overlays (cdr overlays))))
    (if o
        (cons (overlay-start o) (overlay-end o)))))

(defun js2-mode-function-at-point (&optional pos)
  "Return the innermost function node enclosing current point.
Returns nil if point is not in a function."
  (let ((node (js2-node-at-point pos)))
    (while (and node (not (js2-function-node-p node)))
      (setq node (js2-node-parent node)))
    (if (js2-function-node-p node)
        node)))

(defun js2-mode-toggle-element ()
  "Hide or show the foldable element at the point."
  (interactive)
  (let (comment fn pos)
    (save-excursion
      (cond
       ;; /* ... */ comment?
       ((js2-block-comment-p (setq comment (js2-comment-at-point)))
        (if (js2-mode-invisible-overlay-bounds
             (setq pos (+ 3 (js2-node-abs-pos comment))))
            (progn
              (goto-char pos)
              (js2-mode-show-element))
          (js2-mode-hide-element)))
       ;; //-comment?
       ((save-excursion
          (back-to-indentation)
          (looking-at js2-mode-//-comment-re))
        (js2-mode-toggle-//-comment))
       ;; function?
       ((setq fn (js2-mode-function-at-point))
        (setq pos (and (js2-function-node-body fn)
                       (js2-node-abs-pos (js2-function-node-body fn))))
        (goto-char (1+ pos))
        (if (js2-mode-invisible-overlay-bounds)
            (js2-mode-show-element)
          (js2-mode-hide-element)))
       (t
        (message "Nothing at point to hide or show"))))))

(defun js2-mode-hide-element ()
  "Fold/hide contents of a block, showing ellipses.
Show the hidden text with \\[js2-mode-show-element]."
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'js2-mode-hide-element))
  (let (node body beg end)
    (cond
     ((js2-mode-invisible-overlay-bounds)
      (message "already hidden"))
     (t
      (setq node (js2-node-at-point))
      (cond
       ((js2-block-comment-p node)
        (js2-mode-hide-comment node))
       (t
        (while (and node (not (js2-function-node-p node)))
          (setq node (js2-node-parent node)))
        (if (and node
                 (setq body (js2-function-node-body node)))
            (progn
              (setq beg (js2-node-abs-pos body)
                    end (+ beg (js2-node-len body)))
              (js2-mode-flag-region (1+ beg) (1- end) 'hide))
          (message "No collapsable element found at point"))))))))

(defun js2-mode-show-element ()
  "Show the hidden element at current point."
  (interactive)
  (let ((bounds (js2-mode-invisible-overlay-bounds)))
    (if bounds
        (js2-mode-flag-region (car bounds) (cdr bounds) nil)
      (message "Nothing to un-hide"))))

(defun js2-mode-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (js2-mode-flag-region (point-min) (point-max) nil))

(defun js2-mode-toggle-hide-functions ()
  (interactive)
  (if js2-mode-functions-hidden
      (js2-mode-show-functions)
    (js2-mode-hide-functions)))

(defun js2-mode-hide-functions ()
  "Hides all non-nested function bodies in the buffer.
Use \\[js2-mode-show-all] to reveal them, or \\[js2-mode-show-element]
to open an individual entry."
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'js2-mode-hide-functions))
  (if (null js2-mode-ast)
      (message "Oops - parsing failed")
    (setq js2-mode-functions-hidden t)
    (js2-visit-ast js2-mode-ast #'js2-mode-function-hider)))

(defun js2-mode-function-hider (n endp)
  (when (not endp)
    (let ((tt (js2-node-type n))
          body beg end)
      (cond
       ((and (= tt js2-FUNCTION)
             (setq body (js2-function-node-body n)))
        (setq beg (js2-node-abs-pos body)
              end (+ beg (js2-node-len body)))
        (js2-mode-flag-region (1+ beg) (1- end) 'hide)
        nil)   ; don't process children of function
       (t
        t))))) ; keep processing other AST nodes

(defun js2-mode-show-functions ()
  "Un-hide any folded function bodies in the buffer."
  (interactive)
  (setq js2-mode-functions-hidden nil)
  (save-excursion
    (goto-char (point-min))
    (while (/= (goto-char (next-overlay-change (point)))
               (point-max))
      (dolist (o (overlays-at (point)))
        (when (and (overlay-get o 'invisible)
                   (not (overlay-get o 'comment)))
          (js2-mode-flag-region (overlay-start o) (overlay-end o) nil))))))

(defun js2-mode-hide-comment (n)
  (let* ((head (if (eq (js2-comment-node-format n) 'jsdoc)
                   3  ; /**
                 2))  ; /*
         (beg (+ (js2-node-abs-pos n) head))
         (end (- (+ beg (js2-node-len n)) head 2))
         (o (js2-mode-flag-region beg end 'hide)))
    (overlay-put o 'comment t)))

(defun js2-mode-toggle-hide-comments ()
  "Folds all block comments in the buffer.
Use \\[js2-mode-show-all] to reveal them, or \\[js2-mode-show-element]
to open an individual entry."
  (interactive)
  (if js2-mode-comments-hidden
      (js2-mode-show-comments)
    (js2-mode-hide-comments)))

(defun js2-mode-hide-comments ()
  (interactive)
  (if js2-mode-buffer-dirty-p
      (js2-mode-wait-for-parse #'js2-mode-hide-comments))
  (if (null js2-mode-ast)
      (message "Oops - parsing failed")
    (setq js2-mode-comments-hidden t)
    (dolist (n (js2-ast-root-comments js2-mode-ast))
      (when (js2-block-comment-p n)
        (js2-mode-hide-comment n)))
    (js2-mode-hide-//-comments)))

(defun js2-mode-extend-//-comment (direction)
  "Find start or end of a block of similar //-comment lines.
DIRECTION is -1 to look back, 1 to look forward.
INDENT is the indentation level to match.
Returns the end-of-line position of the furthest adjacent
//-comment line with the same indentation as the current line.
If there is no such matching line, returns current end of line."
  (let ((pos (point-at-eol))
        (indent (current-indentation)))
    (save-excursion
      (while (and (zerop (forward-line direction))
                  (looking-at js2-mode-//-comment-re)
                  (eq indent (length (match-string 1))))
        (setq pos (point-at-eol))
      pos))))

(defun js2-mode-hide-//-comments ()
  "Fold adjacent 1-line comments, showing only snippet of first one."
  (let (beg end)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward js2-mode-//-comment-re nil t)
        (setq beg (point)
              end (js2-mode-extend-//-comment 1))
        (unless (eq beg end)
          (overlay-put (js2-mode-flag-region beg end 'hide)
                       'comment t))
        (goto-char end)
        (forward-char 1)))))

(defun js2-mode-toggle-//-comment ()
  "Fold or un-fold any multi-line //-comment at point.
Caller should have determined that this line starts with a //-comment."
  (let* ((beg (point-at-eol))
         (end beg))
    (save-excursion
      (goto-char end)
      (if (js2-mode-invisible-overlay-bounds)
          (js2-mode-show-element)
        ;; else hide the comment
        (setq beg (js2-mode-extend-//-comment -1)
              end (js2-mode-extend-//-comment 1))
        (unless (eq beg end)
          (overlay-put (js2-mode-flag-region beg end 'hide)
                       'comment t))))))

(defun js2-mode-show-comments ()
  "Un-hide any hidden comments, leaving other hidden elements alone."
  (interactive)
  (setq js2-mode-comments-hidden nil)
  (save-excursion
    (goto-char (point-min))
    (while (/= (goto-char (next-overlay-change (point)))
               (point-max))
      (dolist (o (overlays-at (point)))
        (when (overlay-get o 'comment)
          (js2-mode-flag-region (overlay-start o) (overlay-end o) nil))))))

(defun js2-mode-display-warnings-and-errors ()
  "Turn on display of warnings and errors."
  (interactive)
  (setq js2-mode-show-parse-errors t
        js2-mode-show-strict-warnings t)
  (js2-reparse 'force))

(defun js2-mode-hide-warnings-and-errors ()
  "Turn off display of warnings and errors."
  (interactive)
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)
  (js2-reparse 'force))

(defun js2-mode-toggle-warnings-and-errors ()
  "Toggle the display of warnings and errors.
Some users don't like having warnings/errors reported while they type."
  (interactive)
  (setq js2-mode-show-parse-errors (not js2-mode-show-parse-errors)
        js2-mode-show-strict-warnings (not js2-mode-show-strict-warnings))
  (if (called-interactively-p 'any)
      (message "warnings and errors %s"
               (if js2-mode-show-parse-errors
                   "enabled"
                 "disabled")))
  (js2-reparse 'force))

(defun js2-mode-customize ()
  (interactive)
  (customize-group 'js2-mode))

(defun js2-mode-forward-sexp (&optional arg)
  "Move forward across one statement or balanced expression.
With ARG, do it that many times.  Negative arg -N means
move backward across N balanced expressions."
  (interactive "p")
  (setq arg (or arg 1))
  (save-restriction
    (widen) ;; `blink-matching-open' calls `narrow-to-region'
    (js2-reparse)
    (let (forward-sexp-function
          node (start (point)) pos lp rp child)
      (cond
       ;; backward-sexp
       ;; could probably make this better for some cases:
       ;;  - if in statement block (e.g. function body), go to parent
       ;;  - infix exprs like (foo in bar) - maybe go to beginning
       ;;    of infix expr if in the right-side expression?
       ((and arg (minusp arg))
        (dotimes (_ (- arg))
          (js2-backward-sws)
          (forward-char -1)   ; Enter the node we backed up to.
          (when (setq node (js2-node-at-point (point) t))
            (setq pos (js2-node-abs-pos node))
            (let ((parens (js2-mode-forward-sexp-parens node pos)))
              (setq lp (car parens)
                    rp (cdr parens)))
            (when (and lp (> start lp))
              (if (and rp (<= start rp))
                  ;; Between parens, check if there's a child node we can jump.
                  (when (setq child (js2-node-closest-child node (point) lp t))
                    (setq pos (js2-node-abs-pos child)))
                ;; Before both parens.
                (setq pos lp)))
            (let ((state (parse-partial-sexp start pos)))
              (goto-char (if (not (zerop (car state)))
                             ;; Stumble at the unbalanced paren if < 0, or
                             ;; jump a bit further if > 0.
                             (scan-sexps start -1)
                           pos))))
          (unless pos (goto-char (point-min)))))
       (t
        ;; forward-sexp
        (dotimes (_ arg)
          (js2-forward-sws)
          (when (setq node (js2-node-at-point (point) t))
            (setq pos (js2-node-abs-pos node))
            (let ((parens (js2-mode-forward-sexp-parens node pos)))
              (setq lp (car parens)
                    rp (cdr parens)))
            (or
             (when (and rp (<= start rp))
               (if (> start lp)
                   (when (setq child (js2-node-closest-child node (point) rp))
                     (setq pos (js2-node-abs-end child)))
                 (setq pos (1+ rp))))
             ;; No parens or child nodes, looks for the end of the curren node.
             (incf pos (js2-node-len
                        (if (js2-expr-stmt-node-p (js2-node-parent node))
                            ;; Stop after the semicolon.
                            (js2-node-parent node)
                          node))))
            (let ((state (save-excursion (parse-partial-sexp start pos))))
              (goto-char (if (not (zerop (car state)))
                             (scan-sexps start 1)
                           pos))))
          (unless pos (goto-char (point-max)))))))))

(defun js2-mode-forward-sexp-parens (node abs-pos)
  "Return a cons cell with positions of main parens in NODE."
  (cond
   ((or (js2-array-node-p node)
        (js2-object-node-p node)
        (js2-array-comp-node-p node)
        (memq (aref node 0) '(cl-struct-js2-block-node cl-struct-js2-scope)))
    (cons abs-pos (+ abs-pos (js2-node-len node) -1)))
   ((js2-paren-expr-node-p node)
    (let ((lp (js2-node-lp node))
          (rp (js2-node-rp node)))
      (cons (when lp (+ abs-pos lp))
            (when rp (+ abs-pos rp)))))))

(defun js2-node-closest-child (parent point limit &optional before)
  (let* ((parent-pos (js2-node-abs-pos parent))
         (rpoint (- point parent-pos))
         (rlimit (- limit parent-pos))
         (min (min rpoint rlimit))
         (max (max rpoint rlimit))
         found)
    (catch 'done
      (js2-visit-ast
       parent
       (lambda (node _end-p)
         (if (eq node parent)
             t
           (let ((pos (js2-node-pos node)) ;; Both relative values.
                 (end (+ (js2-node-pos node) (js2-node-len node))))
             (when (and (>= pos min) (<= end max)
                        (if before (< pos rpoint) (> end rpoint)))
               (setq found node))
             (when (> end rpoint)
               (throw 'done nil)))
           nil))))
    found))


(defun js2-down-mouse-3 ()
  "Make right-click move the point to the click location.
This makes right-click context menu operations a bit more intuitive.
The point will not move if the region is active, however, to avoid
destroying the region selection."
  (interactive)
  (when (and js2-move-point-on-right-click
             (not mark-active))
    (let ((e last-input-event))
      (ignore-errors
        (goto-char (cadadr e))))))

(defun js2-mode-create-imenu-index ()
  "Return an alist for `imenu--index-alist'."
  ;; This is built up in `js2-parse-record-imenu' during parsing.
  (when js2-mode-ast
    ;; if we have an ast but no recorder, they're requesting a rescan
    (unless js2-imenu-recorder
      (js2-reparse 'force))
    (prog1
        (js2-build-imenu-index)
      (setq js2-imenu-recorder nil
            js2-imenu-function-map nil))))

(defun js2-mode-find-tag ()
  "Replacement for `find-tag-default'.
`find-tag-default' returns a ridiculous answer inside comments."
  (let (beg end)
    (js2-with-underscore-as-word-syntax
      (save-excursion
        (if (and (not (looking-at "[[:alnum:]_$]"))
                 (looking-back "[[:alnum:]_$]"))
            (setq beg (progn (forward-word -1) (point))
                  end (progn (forward-word 1) (point)))
          (setq beg (progn (forward-word 1) (point))
                end (progn (forward-word -1) (point))))
        (replace-regexp-in-string
         "[\"']" ""
         (buffer-substring-no-properties beg end))))))

(defun js2-mode-forward-sibling ()
  "Move to the end of the sibling following point in parent.
Returns non-nil if successful, or nil if there was no following sibling."
  (let* ((node (js2-node-at-point))
         (parent (js2-mode-find-enclosing-fn node))
         sib)
    (when (setq sib (js2-node-find-child-after (point) parent))
      (goto-char (+ (js2-node-abs-pos sib)
                    (js2-node-len sib))))))

(defun js2-mode-backward-sibling ()
  "Move to the beginning of the sibling node preceding point in parent.
Parent is defined as the enclosing script or function."
  (let* ((node (js2-node-at-point))
         (parent (js2-mode-find-enclosing-fn node))
         sib)
    (when (setq sib (js2-node-find-child-before (point) parent))
      (goto-char (js2-node-abs-pos sib)))))

(defun js2-beginning-of-defun (&optional arg)
  "Go to line on which current function starts, and return t on success.
If we're not in a function or already at the beginning of one, go
to beginning of previous script-level element.
With ARG N, do that N times. If N is negative, move forward."
  (setq arg (or arg 1))
  (if (plusp arg)
      (let ((parent (js2-node-parent-script-or-fn (js2-node-at-point))))
        (when (cond
               ((js2-function-node-p parent)
                (goto-char (js2-node-abs-pos parent)))
               (t
                (js2-mode-backward-sibling)))
          (if (> arg 1)
              (js2-beginning-of-defun (1- arg))
            t)))
    (when (js2-end-of-defun)
      (js2-beginning-of-defun (if (>= arg -1) 1 (1+ arg))))))

(defun js2-end-of-defun ()
  "Go to the char after the last position of the current function
or script-level element."
  (let* ((node (js2-node-at-point))
         (parent (or (and (js2-function-node-p node) node)
                     (js2-node-parent-script-or-fn node)))
         script)
    (unless (js2-function-node-p parent)
      ;; Use current script-level node, or, if none, the next one.
      (setq script (or parent node)
            parent (js2-node-find-child-before (point) script))
      (when (or (null parent)
                (>= (point) (+ (js2-node-abs-pos parent)
                               (js2-node-len parent))))
        (setq parent (js2-node-find-child-after (point) script))))
    (when parent
      (goto-char (+ (js2-node-abs-pos parent)
                    (js2-node-len parent))))))

(defun js2-mark-defun (&optional allow-extend)
  "Put mark at end of this function, point at beginning.
The function marked is the one that contains point.

Interactively, if this command is repeated,
or (in Transient Mark mode) if the mark is active,
it marks the next defun after the ones already marked."
  (interactive "p")
  (let (extended)
    (when (and allow-extend
               (or (and (eq last-command this-command) (mark t))
                   (and transient-mark-mode mark-active)))
      (let ((sib (save-excursion
                   (goto-char (mark))
                   (if (js2-mode-forward-sibling)
                       (point)))))
        (if sib
            (progn
              (set-mark sib)
              (setq extended t))
          ;; no more siblings - try extending to enclosing node
          (goto-char (mark t)))))
   (when (not extended)
     (let ((node (js2-node-at-point (point) t)) ; skip comments
           ast fn stmt parent beg end)
       (when (js2-ast-root-p node)
         (setq ast node
               node (or (js2-node-find-child-after (point) node)
                        (js2-node-find-child-before (point) node))))
       ;; only mark whole buffer if we can't find any children
       (if (null node)
           (setq node ast))
       (if (js2-function-node-p node)
           (setq parent node)
         (setq fn (js2-mode-find-enclosing-fn node)
               stmt (if (or (null fn)
                            (js2-ast-root-p fn))
                        (js2-mode-find-first-stmt node))
               parent (or stmt fn)))
       (setq beg (js2-node-abs-pos parent)
             end (+ beg (js2-node-len parent)))
       (push-mark beg)
       (goto-char end)
       (exchange-point-and-mark)))))

(defun js2-narrow-to-defun ()
  "Narrow to the function enclosing point."
  (interactive)
  (let* ((node (js2-node-at-point (point) t))  ; skip comments
         (fn (if (js2-script-node-p node)
                 node
               (js2-mode-find-enclosing-fn node)))
         (beg (js2-node-abs-pos fn)))
    (unless (js2-ast-root-p fn)
      (narrow-to-region beg (+ beg (js2-node-len fn))))))

(provide 'js2-mode)

;;; js2-mode.el ends here
