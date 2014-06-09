;; I originally used symbols to represent tokens, but Rhino uses
;; ints and then sets various flag bits in them, so ints it is.
;; The upshot is that we need a `js2-' prefix in front of each name.
(defvar js2-ERROR -1)
(defvar js2-EOF 0)
(defvar js2-EOL 1)
(defvar js2-ENTERWITH 2)       ; begin interpreter bytecodes
(defvar js2-LEAVEWITH 3)
(defvar js2-RETURN 4)
(defvar js2-GOTO 5)
(defvar js2-IFEQ 6)
(defvar js2-IFNE 7)
(defvar js2-SETNAME 8)
(defvar js2-BITOR 9)
(defvar js2-BITXOR 10)
(defvar js2-BITAND 11)
(defvar js2-EQ 12)
(defvar js2-NE 13)
(defvar js2-LT 14)
(defvar js2-LE 15)
(defvar js2-GT 16)
(defvar js2-GE 17)
(defvar js2-LSH 18)
(defvar js2-RSH 19)
(defvar js2-URSH 20)
(defvar js2-ADD 21)            ; infix plus
(defvar js2-SUB 22)            ; infix minus
(defvar js2-MUL 23)
(defvar js2-DIV 24)
(defvar js2-MOD 25)
(defvar js2-NOT 26)
(defvar js2-BITNOT 27)
(defvar js2-POS 28)            ; unary plus
(defvar js2-NEG 29)            ; unary minus
(defvar js2-NEW 30)
(defvar js2-DELPROP 31)
(defvar js2-TYPEOF 32)
(defvar js2-GETPROP 33)
(defvar js2-GETPROPNOWARN 34)
(defvar js2-SETPROP 35)
(defvar js2-GETELEM 36)
(defvar js2-SETELEM 37)
(defvar js2-CALL 38)
(defvar js2-NAME 39)           ; an identifier
(defvar js2-NUMBER 40)
(defvar js2-STRING 41)
(defvar js2-NULL 42)
(defvar js2-THIS 43)
(defvar js2-FALSE 44)
(defvar js2-TRUE 45)
(defvar js2-SHEQ 46)           ; shallow equality (===)
(defvar js2-SHNE 47)           ; shallow inequality (!==)
(defvar js2-REGEXP 48)
(defvar js2-BINDNAME 49)
(defvar js2-THROW 50)
(defvar js2-RETHROW 51)        ; rethrow caught exception: catch (e if ) uses it
(defvar js2-IN 52)
(defvar js2-INSTANCEOF 53)
(defvar js2-LOCAL_LOAD 54)
(defvar js2-GETVAR 55)
(defvar js2-SETVAR 56)
(defvar js2-CATCH_SCOPE 57)
(defvar js2-ENUM_INIT_KEYS 58)
(defvar js2-ENUM_INIT_VALUES 59)
(defvar js2-ENUM_INIT_ARRAY 60)
(defvar js2-ENUM_NEXT 61)
(defvar js2-ENUM_ID 62)
(defvar js2-THISFN 63)
(defvar js2-RETURN_RESULT 64)  ; to return previously stored return result
(defvar js2-ARRAYLIT 65)       ; array literal
(defvar js2-OBJECTLIT 66)      ; object literal
(defvar js2-GET_REF 67)        ; *reference
(defvar js2-SET_REF 68)        ; *reference = something
(defvar js2-DEL_REF 69)        ; delete reference
(defvar js2-REF_CALL 70)       ; f(args) = something or f(args)++
(defvar js2-REF_SPECIAL 71)    ; reference for special properties like __proto
(defvar js2-YIELD 72)          ; JS 1.7 yield pseudo keyword

;; XML support
(defvar js2-DEFAULTNAMESPACE 73)
(defvar js2-ESCXMLATTR 74)
(defvar js2-ESCXMLTEXT 75)
(defvar js2-REF_MEMBER 76)     ; Reference for x.@y, x..y etc.
(defvar js2-REF_NS_MEMBER 77)  ; Reference for x.ns::y, x..ns::y etc.
(defvar js2-REF_NAME 78)       ; Reference for @y, @[y] etc.
(defvar js2-REF_NS_NAME 79)    ; Reference for ns::y, @ns::y@[y] etc.

(defvar js2-first-bytecode js2-ENTERWITH)
(defvar js2-last-bytecode js2-REF_NS_NAME)

(defvar js2-TRY 80)
(defvar js2-SEMI 81)           ; semicolon
(defvar js2-LB 82)             ; left and right brackets
(defvar js2-RB 83)
(defvar js2-LC 84)             ; left and right curly-braces
(defvar js2-RC 85)
(defvar js2-LP 86)             ; left and right parens
(defvar js2-RP 87)
(defvar js2-COMMA 88)          ; comma operator

(defvar js2-ASSIGN 89)         ; simple assignment (=)
(defvar js2-ASSIGN_BITOR 90)   ; |=
(defvar js2-ASSIGN_BITXOR 91)  ; ^=
(defvar js2-ASSIGN_BITAND 92)  ; &=
(defvar js2-ASSIGN_LSH 93)     ; <<=
(defvar js2-ASSIGN_RSH 94)     ; >>=
(defvar js2-ASSIGN_URSH 95)    ; >>>=
(defvar js2-ASSIGN_ADD 96)     ; +=
(defvar js2-ASSIGN_SUB 97)     ; -=
(defvar js2-ASSIGN_MUL 98)     ; *=
(defvar js2-ASSIGN_DIV 99)     ; /=
(defvar js2-ASSIGN_MOD 100)    ; %=

(defvar js2-first-assign js2-ASSIGN)
(defvar js2-last-assign js2-ASSIGN_MOD)

(defvar js2-HOOK 101)          ; conditional (?:)
(defvar js2-COLON 102)
(defvar js2-OR 103)            ; logical or (||)
(defvar js2-AND 104)           ; logical and (&&)
(defvar js2-INC 105)           ; increment/decrement (++ --)
(defvar js2-DEC 106)
(defvar js2-DOT 107)           ; member operator (.)
(defvar js2-FUNCTION 108)      ; function keyword
(defvar js2-EXPORT 109)        ; export keyword
(defvar js2-IMPORT 110)        ; import keyword
(defvar js2-IF 111)            ; if keyword
(defvar js2-ELSE 112)          ; else keyword
(defvar js2-SWITCH 113)        ; switch keyword
(defvar js2-CASE 114)          ; case keyword
(defvar js2-DEFAULT 115)       ; default keyword
(defvar js2-WHILE 116)         ; while keyword
(defvar js2-DO 117)            ; do keyword
(defvar js2-FOR 118)           ; for keyword
(defvar js2-BREAK 119)         ; break keyword
(defvar js2-CONTINUE 120)      ; continue keyword
(defvar js2-VAR 121)           ; var keyword
(defvar js2-WITH 122)          ; with keyword
(defvar js2-CATCH 123)         ; catch keyword
(defvar js2-FINALLY 124)       ; finally keyword
(defvar js2-VOID 125)          ; void keyword
(defvar js2-RESERVED 126)      ; reserved keywords

(defvar js2-EMPTY 127)

;; Types used for the parse tree - never returned by scanner.

(defvar js2-BLOCK 128)         ; statement block
(defvar js2-LABEL 129)         ; label
(defvar js2-TARGET 130)
(defvar js2-LOOP 131)
(defvar js2-EXPR_VOID 132)     ; expression statement in functions
(defvar js2-EXPR_RESULT 133)   ; expression statement in scripts
(defvar js2-JSR 134)
(defvar js2-SCRIPT 135)        ; top-level node for entire script
(defvar js2-TYPEOFNAME 136)    ; for typeof(simple-name)
(defvar js2-USE_STACK 137)
(defvar js2-SETPROP_OP 138)    ; x.y op= something
(defvar js2-SETELEM_OP 139)    ; x[y] op= something
(defvar js2-LOCAL_BLOCK 140)
(defvar js2-SET_REF_OP 141)    ; *reference op= something

;; For XML support:
(defvar js2-DOTDOT 142)        ; member operator (..)
(defvar js2-COLONCOLON 143)    ; namespace::name
(defvar js2-XML 144)           ; XML type
(defvar js2-DOTQUERY 145)      ; .() -- e.g., x.emps.emp.(name == "terry")
(defvar js2-XMLATTR 146)       ; @
(defvar js2-XMLEND 147)

;; Optimizer-only tokens
(defvar js2-TO_OBJECT 148)
(defvar js2-TO_DOUBLE 149)

(defvar js2-GET 150)           ; JS 1.5 get pseudo keyword
(defvar js2-SET 151)           ; JS 1.5 set pseudo keyword
(defvar js2-LET 152)           ; JS 1.7 let pseudo keyword
(defvar js2-CONST 153)
(defvar js2-SETCONST 154)
(defvar js2-SETCONSTVAR 155)
(defvar js2-ARRAYCOMP 156)
(defvar js2-LETEXPR 157)
(defvar js2-WITHEXPR 158)
(defvar js2-DEBUGGER 159)

(defvar js2-COMMENT 160)
(defvar js2-ENUM 161)          ; for "enum" reserved word
(defvar js2-TRIPLEDOT 162)     ; for rest parameter
(defvar js2-ARROW 163)         ; function arrow (=>)

(defconst js2-num-tokens (1+ js2-ARROW))

(defconst js2-debug-print-trees nil)

(provide 'js2-tokens)
