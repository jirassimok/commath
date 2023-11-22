;;; commath.el --- Non-prefix math using special comma syntax.

;; Copyright (C) 2023 Jacob Komissar

;; Author: Jacob Komissar <18540209+jirassimok@users.noreply.github.com>
;; Version: 1.0
;; Package-Requires: (dash)
;; Keywords: extensions, lisp

;;; Commentary:
;;
;; This library implements a macro that allows infix math notation in
;; Emacs Lisp. The macro is named `\,' (or `commath') and it rewrites
;; certain types of expressions ("commath expressions") as Emacs Lisp
;; expressions. See the documentation of the macro for details.
;;
;; This is meant as a convenience for writing and reading long math
;; expressions.
;;
;; TODO:
;; - Auto-group and/or/+/* operations.
;;   - Convert subtraction to negation and addition.
;; - Add customizable operator list.
;; - Add chainable comparisons.
;; - Add constants.
;; - Ban `1-' for being confusing.
;; - Better error messages.

;;; Code:
(require 'dash)

(defmacro \, (&rest expr)
  "Perform math in non-prefix notation.

This macro allows infix math operations and comparisons, formed
from a limited set of expression types, refered to as \"commath
expressions.\"

There are five types of commath expression. First is a simple
value, which may be a number or variable name. These evaluate
to their values. Second is X OP Y, where X and Y are commath
expressions, and OP is an infix operator from the list below.
There must be spaces around all infix operators.

Third is grouping, which may use either parentheses or brackets,
i.e. (EXPR) or [EXPR]. If the grouping symbols are left out of an
expression, it will be implicitly grouped according to standard
operator precedence and associativity, similar to C.

The fourth type of commath expression is a function call, of the
form NAME(ARG1, ARG2, ...). NAME is an Emacs Lisp function, and
ARG1, ARG2, and so on are commath expressions to use as its
arguments.

The final type of commath expression is a quoted value, which
will not be evaluated. This allows passing symbols and
non-numeric Emacs Lisp literals as arguments to functions if
necessary.

The allowed operators are `+', `-', `*', `/', `%', `mod', `<',
`>', `<=', `>=', `/=', `and', `or', and `^'. These expand to the
Emacs Lisp functions of the same names, except for `^', which
expands to `expt'."
  (pcase (length expr)
    (1 `(\,-simple-expr ,(car expr)))
    (2 `(\,-fn-expr ,(car expr) ,(cadr expr)))
    (3 `(\,-op-expr ,(car expr) ,(cadr expr) ,(caddr expr)))
    (_ (\,-group-precedence expr))))

(defalias 'commath '\,)

(defun \,-token-type (token)
  "Simple token checker. Performs naive token type check
and returns unprocessed token types."
  (pcase token
    ((pred numberp) 'number)
    ((or '+ '- '* '/ '% '< '> '<= '>= '/= '^ 'and 'or 'mod) 'operator)
    ((pred vectorp) 'vector-group)
    ((pred symbolp) 'name)
    ;; Other non-cons values are not recognized tokens.
    ((pred (not consp)) nil)
    ;; 'quote means it's quoted for literal escape
    ((app car 'quote) 'quoted)
    ((app car (or '\, '\` '\,@)) nil)
    ;; other cons must be group or args
    (_ 'group-or-args)))

(defmacro \,-simple-expr (arg)
  "Expand single-argument commath input.

This must be a number, variable name, or group."
  (pcase (\,-token-type arg)
    ('number arg)
    ('name arg)
    ('vector-group (cons '\, (append arg nil)))
    ('group-or-args (cons '\, arg))
    ('quoted arg)
    ('operator (error "Operator (%s) expected two arguments." arg))
    (_ (error "Invalid type for commath expression."))))

(defmacro \,-fn-expr (fname args)
  "Expand two-argument commath input.

This must be a function call."
  (let ((t1 (\,-token-type fname))
        (t2 (\,-token-type args)))
    (when (memq 'operator (list t1 t2))
      (let ((name (if (eq 'operator t1) fname args)))
        (error "Operator (%s) expected two arguments." name)))
    (if (and (eq t1 'name) (eq t2 'group-or-args))
        (cons fname (--map (cons '\, it) (\,-split-fn-args args)))
      (error "Invalid commath expression."))))

(defmacro \,-op-expr (arg1 op arg2)
  "Expand three-argument commath input.

This must be an operator expression."
  (unless (eq (\,-token-type op) 'operator)
    ;; TODO: Better error messages.
    (error "Invalid commath expression (%s %s %s)." arg1 op arg2))
  ;; TODO: Make special operator alist to define more operators
  (when (eq op '^) (setq op 'expt))
  (list op (list '\, arg1) (list '\, arg2)))

(defun \,-group-precedence (tokens)
  "Simplify commath expressions by grouping by operator precedence.

This is not implemented efficiently, but is performed only during
macro expansion.

TOKENS is the list of tokens to group. This list may be
destructively modified."
  (\,--group-precedence 'right tokens
   '((left and)
     (left or)
     (left < > <= >= = /=)
     (left + -)
     (left * / % mod)
     (right ^))))

(defun \,--group-precedence (dir tokens remaining-ops)
  "Handle associativity by reversing the tokens at certain points.

Regardless of initial order, when no operators remain, any tokens
that remain ungrouped will be in the initial order."
  (-let [((assoc . ops) . later-ops) remaining-ops]
    (cond
     ((null ops)
      (list '\, (if (eq dir 'left)
                    (reverse tokens)
                  tokens)))
     ((--none? (memq it ops) tokens) ;; operator not present
      (\,--group-precedence dir tokens later-ops))
     (t ;; operator is present
      (unless (eq dir assoc)
        ;; Can't use nreverse because group-operator uses the tail of
        ;; the list
        (setq tokens (reverse tokens)))
      (-let [(op right left) (\,-group-operator ops tokens)]
        (when (eq assoc 'right)
          (\,--swap left right))
        (when (null op)
          ;; no op found despite earlier check
          (error "Op not found, then found."))
        ;; op found, recur on each side
        (list op
              (\,--group-precedence assoc left remaining-ops)
              (\,--group-precedence assoc right remaining-ops)))))))

(defun \,-group-operator (ops tokens)
  "Scan list and split around an operator from the list,
returning a list of the operator, left, and right arguments
(assuming they were in right-associative order).

If no operator is found, the operator and right argument will be
nil."
  (-let [(left right) (--split-with (not (memq it ops)) tokens)]
    (list (car right) left (cdr right))))

(defmacro \,--swap (a b)
  "Swap two variables."
  `(setq ,a (prog1 ,b (setq ,b ,a))))

(defun \,-split-fn-args (tokens)
  "Split function arguments into separate commath expressions."
  (when (and (consp (car tokens)) (eq '\, (caar tokens)))
    (error "Comma before first function argument"))
  (let ((args (-partition-before-pred
               (lambda (it)
                 (and (consp it) (eq '\, (car it))))
               tokens)))
    (cons (car args)
          ;; Each subsequent arg is ((, x) y z), so we
          ;; extract and combine the x and the (y z).
          (--map (cons (cadar it) (cdr it))
                 (cdr args)))))

;; Local Variables:
;; lexical-binding: t;
;; read-symbol-shorthands: ((",-" . "commath-"));
;; End:
;;; commath.el ends here
