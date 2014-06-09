(defmacro js2-deflocal (name value &optional comment)
  "Define a buffer-local variable NAME with VALUE and COMMENT."
  `(progn
     (defvar ,name ,value ,comment)
     (make-variable-buffer-local ',name)))


(defun js2-mark-safe-local (name pred)
  "Make the variable NAME buffer-local and mark it as safe file-local
variable with predicate PRED."
  (make-variable-buffer-local name)
  (put name 'safe-local-variable pred))

(defcustom js2-highlight-level 2
  "Amount of syntax highlighting to perform.
0 or a negative value means none.
1 adds basic syntax highlighting.
2 adds highlighting of some Ecma built-in properties.
3 adds highlighting of many Ecma built-in functions."
  :group 'js2-mode
  :type '(choice (const :tag "None" 0)
                 (const :tag "Basic" 1)
                 (const :tag "Include Properties" 2)
                 (const :tag "Include Functions" 3)))

(defvar js2-mode-dev-mode-p nil
  "Non-nil if running in development mode.  Normally nil.")

(defgroup js2-mode nil
  "An improved JavaScript mode."
  :group 'languages)

(defcustom js2-basic-offset (if (and (boundp 'c-basic-offset)
                                     (numberp c-basic-offset))
                                c-basic-offset
                              4)
  "Number of spaces to indent nested statements.
Similar to `c-basic-offset'."
  :group 'js2-mode
  :type 'integer)
(js2-mark-safe-local 'js2-basic-offset 'integerp)

(defcustom js2-bounce-indent-p nil
  "Non-nil to have indent-line function choose among alternatives.
If nil, the indent-line function will indent to a predetermined column
based on heuristic guessing.  If non-nil, then if the current line is
already indented to that predetermined column, indenting will choose
another likely column and indent to that spot.  Repeated invocation of
the indent-line function will cycle among the computed alternatives.
See the function `js2-bounce-indent' for details.  When it is non-nil,
js2-mode also binds `js2-bounce-indent-backwards' to Shift-Tab."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-pretty-multiline-declarations t
  "Non-nil to line up multiline declarations vertically:

  var a = 10,
      b = 20,
      c = 30;

If the value is not `all', and the first assigned value in
declaration is a function/array/object literal spanning several
lines, it won't be indented additionally:

  var o = {                   var bar = 2,
    foo: 3          vs.           o = {
  },                                foo: 3
      bar = 2;                    };"
  :group 'js2-mode
  :type 'symbol)
(js2-mark-safe-local 'js2-pretty-multiline-declarations 'symbolp)

(defcustom js2-indent-switch-body nil
  "When nil, case labels are indented on the same level as the
containing switch statement.  Otherwise, all lines inside
switch statement body are indented one additional level."
  :type 'boolean
  :group 'js2-mode)
(js2-mark-safe-local 'js2-indent-case-same-as-switch 'booleanp)

(defcustom js2-idle-timer-delay 0.2
  "Delay in secs before re-parsing after user makes changes.
Multiplied by `js2-dynamic-idle-timer-adjust', which see."
  :type 'number
  :group 'js2-mode)
(make-variable-buffer-local 'js2-idle-timer-delay)

(defcustom js2-dynamic-idle-timer-adjust 0
  "Positive to adjust `js2-idle-timer-delay' based on file size.
The idea is that for short files, parsing is faster so we can be
more responsive to user edits without interfering with editing.
The buffer length in characters (typically bytes) is divided by
this value and used to multiply `js2-idle-timer-delay' for the
buffer.  For example, a 21k file and 10k adjust yields 21k/10k
== 2, so js2-idle-timer-delay is multiplied by 2.
If `js2-dynamic-idle-timer-adjust' is 0 or negative,
`js2-idle-timer-delay' is not dependent on the file size."
  :type 'number
  :group 'js2-mode)

(defcustom js2-concat-multiline-strings t
  "When non-nil, `js2-line-break' in mid-string will make it a
string concatenation. When `eol', the '+' will be inserted at the
end of the line, otherwise, at the beginning of the next line."
  :type '(choice (const t) (const eol) (const nil))
  :group 'js2-mode)

(defcustom js2-mode-show-parse-errors t
  "True to highlight parse errors."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-mode-show-strict-warnings t
  "Non-nil to emit Ecma strict-mode warnings.
Some of the warnings can be individually disabled by other flags,
even if this flag is non-nil."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-trailing-comma-warning t
  "Non-nil to warn about trailing commas in array literals.
Ecma-262-5.1 allows them, but older versions of IE raise an error."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-missing-semi-warning t
  "Non-nil to warn about semicolon auto-insertion after statement.
Technically this is legal per Ecma-262, but some style guides disallow
depending on it."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-missing-semi-one-line-override nil
  "Non-nil to permit missing semicolons in one-line functions.
In one-liner functions such as `function identity(x) {return x}'
people often omit the semicolon for a cleaner look.  If you are
such a person, you can suppress the missing-semicolon warning
by setting this variable to t."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-inconsistent-return-warning t
  "Non-nil to warn about mixing returns with value-returns.
It's perfectly legal to have a `return' and a `return foo' in the
same function, but it's often an indicator of a bug, and it also
interferes with type inference (in systems that support it.)"
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-cond-assign-warning t
  "Non-nil to warn about expressions like if (a = b).
This often should have been '==' instead of '='.  If the warning
is enabled, you can suppress it on a per-expression basis by
parenthesizing the expression, e.g. if ((a = b)) ..."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-var-redeclaration-warning t
  "Non-nil to warn about redeclaring variables in a script or function."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-strict-var-hides-function-arg-warning t
  "Non-nil to warn about a var decl hiding a function argument."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-skip-preprocessor-directives nil
  "Non-nil to treat lines beginning with # as comments.
Useful for viewing Mozilla JavaScript source code."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-language-version 200
  "Configures what JavaScript language version to recognize.
Currently versions 150, 160, 170, 180 and 200 are supported,
corresponding to JavaScript 1.5, 1.6, 1.7, 1.8 and 2.0 (Harmony),
respectively.  In a nutshell, 1.6 adds E4X support, 1.7 adds let,
yield, and Array comprehensions, and 1.8 adds function closures."
  :type 'integer
  :group 'js2-mode)

(defcustom js2-allow-keywords-as-property-names t
  "If non-nil, you can use JavaScript keywords as object property names.
Examples:

  var foo = {int: 5, while: 6, continue: 7};
  foo.return = 8;

Ecma-262 5.1 allows this syntax, but some engines still don't."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-instanceof-has-side-effects nil
  "If non-nil, treats the instanceof operator as having side effects.
This is useful for xulrunner apps."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-move-point-on-right-click t
  "Non-nil to move insertion point when you right-click.
This makes right-click context menu behavior a bit more intuitive,
since menu operations generally apply to the point.  The exception
is if there is a region selection, in which case the point does -not-
move, so cut/copy/paste can work properly.

Note that IntelliJ moves the point, and Eclipse leaves it alone,
so this behavior is customizable."
  :group 'js2-mode
  :type 'boolean)

(defcustom js2-allow-rhino-new-expr-initializer t
  "Non-nil to support a Rhino's experimental syntactic construct.

Rhino supports the ability to follow a `new' expression with an object
literal, which is used to set additional properties on the new object
after calling its constructor.  Syntax:

  new <expr> [ ( arglist ) ] [initializer]

Hence, this expression:

  new Object {a: 1, b: 2}

results in an Object with properties a=1 and b=2.  This syntax is
apparently not configurable in Rhino - it's currently always enabled,
as of Rhino version 1.7R2."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-allow-member-expr-as-function-name nil
  "Non-nil to support experimental Rhino syntax for function names.

Rhino supports an experimental syntax configured via the Rhino Context
setting `allowMemberExprAsFunctionName'.  The experimental syntax is:

  function <member-expr> ( [ arg-list ] ) { <body> }

Where member-expr is a non-parenthesized 'member expression', which
is anything at the grammar level of a new-expression or lower, meaning
any expression that does not involve infix or unary operators.

When <member-expr> is not a simple identifier, then it is syntactic
sugar for assigning the anonymous function to the <member-expr>.  Hence,
this code:

  function a.b().c[2] (x, y) { ... }

is rewritten as:

  a.b().c[2] = function(x, y) {...}

which doesn't seem particularly useful, but Rhino permits it."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-init-hook nil
  "List of functions to be called after `js2-mode' or
`js2-minor-mode' has initialized all variables, before parsing
the buffer for the first time."
  :type 'hook
  :group 'js2-mode
  :version "20130608")

(defcustom js2-post-parse-callbacks nil
  "List of callback functions invoked after parsing finishes.
Currently, the main use for this function is to add synthetic
declarations to `js2-recorded-identifiers', which see."
  :type 'hook
  :group 'js2-mode)

(defcustom js2-mode-indent-inhibit-undo nil
  "Non-nil to disable collection of Undo information when indenting lines.
Some users have requested this behavior.  It's nil by default because
other Emacs modes don't work this way."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-mode-indent-ignore-first-tab nil
  "If non-nil, ignore first TAB keypress if we look indented properly.
It's fairly common for users to navigate to an already-indented line
and press TAB for reassurance that it's been indented.  For this class
of users, we want the first TAB press on a line to be ignored if the
line is already indented to one of the precomputed alternatives.

This behavior is only partly implemented.  If you TAB-indent a line,
navigate to another line, and then navigate back, it fails to clear
the last-indented variable, so it thinks you've already hit TAB once,
and performs the indent.  A full solution would involve getting on the
point-motion hooks for the entire buffer.  If we come across another
use cases that requires watching point motion, I'll consider doing it.

If you set this variable to nil, then the TAB key will always change
the indentation of the current line, if more than one alternative
indentation spot exists."
  :type 'boolean
  :group 'js2-mode)

(defvar js2-indent-hook nil
  "A hook for user-defined indentation rules.

Functions on this hook should expect two arguments:    (LIST INDEX)
The LIST argument is the list of computed indentation points for
the current line.  INDEX is the list index of the indentation point
that `js2-bounce-indent' plans to use.  If INDEX is nil, then the
indent function is not going to change the current line indentation.

If a hook function on this list returns a non-nil value, then
`js2-bounce-indent' assumes the hook function has performed its own
indentation, and will do nothing.  If all hook functions on the list
return nil, then `js2-bounce-indent' will use its computed indentation
and reindent the line.

When hook functions on this hook list are called, the variable
`js2-mode-ast' may or may not be set, depending on whether the
parse tree is available.  If the variable is nil, you can pass a
callback to `js2-mode-wait-for-parse', and your callback will be
called after the new parse tree is built.  This can take some time
in large files.")


(defcustom js2-highlight-external-variables t
  "Non-nil to highlight undeclared variable identifiers.
An undeclared variable is any variable not declared with var or let
in the current scope or any lexically enclosing scope.  If you use
such a variable, then you are either expecting it to originate from
another file, or you've got a potential bug."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-include-jslint-globals t
  "Non-nil to include the identifiers from JSLint global
declaration (see http://www.jslint.com/lint.html#global) in the
buffer-local externs list.  See `js2-additional-externs' for more
information."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-global-externs nil
  "A list of any extern names you'd like to consider always declared.
This list is global and is used by all `js2-mode' files.
You can create buffer-local externs list using `js2-additional-externs'.

There is also a buffer-local variable `js2-default-externs',
which is initialized by default to include the Ecma-262 externs
and the standard browser externs.  The three lists are all
checked during highlighting."
  :type 'list
  :group 'js2-mode)

(js2-deflocal js2-default-externs nil
  "Default external declarations.

These are currently only used for highlighting undeclared variables,
which only worries about top-level (unqualified) references.
As js2-mode's processing improves, we will flesh out this list.

The initial value is set to `js2-ecma-262-externs', unless some
of the `js2-include-?-externs' variables are set to t, in which
case the browser, Rhino and/or Node.js externs are also included.

See `js2-additional-externs' for more information.")

(defcustom js2-include-browser-externs t
  "Non-nil to include browser externs in the master externs list.
If you work on JavaScript files that are not intended for browsers,
such as Mozilla Rhino server-side JavaScript, set this to nil.
See `js2-additional-externs' for more information about externs."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-include-rhino-externs nil
  "Non-nil to include Mozilla Rhino externs in the master externs list.
See `js2-additional-externs' for more information about externs."
  :type 'boolean
  :group 'js2-mode)

(defcustom js2-include-node-externs nil
  "Non-nil to include Node.js externs in the master externs list.
See `js2-additional-externs' for more information about externs."
  :type 'boolean
  :group 'js2-mode)

(js2-deflocal js2-additional-externs nil
  "A buffer-local list of additional external declarations.
It is used to decide whether variables are considered undeclared
for purposes of highlighting.

Each entry is a Lisp string.  The string should be the fully qualified
name of an external entity.  All externs should be added to this list,
so that as js2-mode's processing improves it can take advantage of them.

You may want to declare your externs in three ways.
First, you can add externs that are valid for all your JavaScript files.
You should probably do this by adding them to `js2-global-externs', which
is a global list used for all js2-mode files.

Next, you can add a function to `js2-init-hook' that adds additional
externs appropriate for the specific file, perhaps based on its path.
These should go in `js2-additional-externs', which is buffer-local.

Third, you can use JSLint's global declaration, as long as
`js2-include-jslint-globals' is non-nil, which see.

Finally, you can add a function to `js2-post-parse-callbacks',
which is called after parsing completes, and `js2-mode-ast' is bound to
the root of the parse tree.  At this stage you can set up an AST
node visitor using `js2-visit-ast' and examine the parse tree
for specific import patterns that may imply the existence of
other externs, possibly tied to your build system.  These should also
be added to `js2-additional-externs'.

Your post-parse callback may of course also use the simpler and
faster (but perhaps less robust) approach of simply scanning the
buffer text for your imports, using regular expressions.")

(provide 'js2-custom)
