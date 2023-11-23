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
;; - Add explicitly customizable operator list.
;; - Add chainable comparisons.
;; - Add explicitly customizable constants.
;; - Add ability to override constants.
;; - Better error messages.
;; - Make `describe-function' report `\,' as a macro
;; - Fix group-precedence to rewrite instead of expanding operators
;;   fully

;;; Code:
(require 'backquote) ;; for docstring reasons
(require 'dash)

(defconst \,-constants
  '((pi . float-pi)
    (e . float-e))
  "Constants for commath expressions. These are always rewritten to
their associated values in commath expressions.

For example ,\(pi + 1) expands to \(+ float-pi 1).")
;; Constants are /always/ rewritten. Ideally, we would want to expand
;; them only when they don't have a different definition, but we can't
;; know that when the expansion takes place, so they are
;; unconditional. One alternative would be to expand constants like
;; this: (if (boundp 'e) e float-e).

(defconst \,-operator-function-alist
  '((^ . expt))
  "List associating operators with their functions.

Any operator not in this list is associated with itself.")

(defconst \,-operator-rules
  '((left and)
    (left or)
    (left < > <= >= = /=)
    (left + -)
    (left * / % mod)
    (right ^))
  "List of commath operators ordered by precedence and tagged by
associativity.")

(defconst \,-operators (-mapcat 'cdr \,-operator-rules)
  "List of allowed operators in commath.")

(defmacro commath (&rest expr)
  "Rewrite math EXPRESSIONs as Lisp forms.

This macro allows infix math operations and comparisons, formed
from a limited set of expression types, refered to as \"commath
expressions.\"

There are five types of commath expression. First is a simple
value, which may be a number or variable name. These are not
rewritten by commath, except for `pi' and `e', which expand to
`float-pi' and `float-e' respectively.

Second is X OP Y, where X and Y are commath expressions, and OP
is an infix operator from the list below. There must be spaces
around all infix operators.

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
expands to `expt'.

Here is an example demonstrating all of these features:
    ,(1 / 2 * (a - 3 ^ [x / 4] ^ 5)
      * nth(2, mapcar('fn, '(1 2 3))))

That expands to the following:
    (* (* (/ 1 2)
          (- a (expt 3
                     (expt (/ x 4)
                           5))))
       (nth 6 (mapcar 'fn '(1 2 3))))

\(fn EXPRESSION)"
  (pcase (length expr)
    (1 `(\,-simple-expr ,(car expr)))
    (2 `(\,-fn-expr ,(car expr) ,(cadr expr)))
    (3 `(\,-op-expr ,(car expr) ,(cadr expr) ,(caddr expr)))
    (_ (\,-group-precedence 'right expr \,-operator-rules))))

;; Define like `\`'; `\,' appears as its own object, not an alias.
(defalias '\, (symbol-function 'commath))

(defun \,-token-type (token)
  "Simple token checker. Performs naive token type check
and returns unprocessed token types."
  (pcase token
    ((pred numberp) 'number)
    ((pred (seq-contains-p \,-operators)) 'operator)
    ((pred vectorp) 'vector-group)
    ;; (Would use assq below, but wrong arg order.)
    ((pred (map-contains-key \,-constants)) 'constant)
    ((pred symbolp) 'name)
    ;; Other non-cons values are not recognized tokens.
    ((pred (not consp)) nil)
    ;; 'quote means it's quoted for literal escape
    ((app car 'quote) 'quoted)
    ((app car (or '\, '\` '\,@)) nil)
    ;; other cons must be group or args
    (_ 'group-or-args)))

;; Convenience functions for creating commath forms in macros
(defun \,--wrap (arg)
  "Put a comma in front of something."
  (list '\, arg))

(defun \,--ify (arg) ;; "commathify"
  "Turn a form into a comma form."
  (cons '\, arg))

(defmacro \,-simple-expr (arg)
  "Expand single-argument commath input.

This must be a number, variable name, or group."
  (pcase (\,-token-type arg)
    ('number arg)
    ('name arg)
    ('constant (cdr (assq arg \,-constants)))
    ('vector-group (\,--ify (append arg nil)))
    ('group-or-args (\,--ify arg))
    ('quoted arg)
    ('operator (error "Operator (%s) expected two arguments." arg))
    (_ (error "Invalid type for commath expression."))))

(defmacro \,-fn-expr (fname args)
  "Expand two-argument commath input.

This must be a function call."
  (let ((tname (\,-token-type fname))
        (targs (\,-token-type args)))
    (cond
     ;; Correct case:
     ((and (eq tname 'name) (eq targs 'group-or-args))
      (when (eq fname '1-)
        (error "Illegal commath expression; use (x - 1) instead of (1-(x))."))
      (cons fname (--map (\,--ify it) (\,-split-fn-args args))))
     ;; Bad expression types:
     ((eq 'operator tname)
      (error "Operator (%s) expected two arguments." fname))
     ((eq 'operator targs)
      (error "Operator (%s) expected two arguments." args))
     ((or (null tname) (null targs))
      (error "Invalid type for commath expression (%s)."
             (if tname args fname)))
     (:else
      (error "Invalid commath expression (%s %s)." fname args)))))

(defmacro \,-op-expr (arg1 op arg2)
  "Expand three-argument commath input.

This must be an operator expression."
  (unless (eq (\,-token-type op) 'operator)
    ;; TODO: Better error messages.
    (error "Invalid commath expression (%s %s %s)." arg1 op arg2))
  (setq op (alist-get op \,-operator-function-alist op))
  (list op (\,--wrap arg1) (\,--wrap arg2)))

(defun \,-group-precedence (dir tokens remaining-ops)
  "Simplify commath expression TOKENS by grouping around operators.

DIR is the associativity direction of the tokens, which is
normally `right', or `left' for tokens in reversed order.
Regardless of initial order, when no operators remain, any tokens
that remain ungrouped will be in `right'-associative order.

REMAINING-OPS is a list of operators in the format of
`commath-operator-rules'.

The list of TOKENS may be destructively modified."
  ;; This call will search for the first set of ops in the list.
  (-let [((assoc . ops) . later-ops) remaining-ops]
    (cond
     ;; No operators left: return tokens in `right' order
     ((null ops)
      (\,--wrap (if (eq dir 'left) (reverse tokens) tokens)))
     ;; Target operator not present: search for rest.
     ((--none? (memq it ops) tokens)
      (\,-group-precedence dir tokens later-ops))
     ;; Target operator is present
     (:else
      (unless (eq dir assoc)
        (setq tokens (reverse tokens)))
      (-let [(left op right) (\,--group-operator ops tokens)]
        (when (eq assoc 'left)
          ;; In left-associative order, "before operator" means right,
          ;; and "after operator" means left, so swap.
          (\,--swap left right))
        (when (null op) ;; no op found despite earlier check
          (error "Op not found, then found."))
        ;; op found, recur on each side
        ;; TODO: This should be a rewrite, not expansion.
        (list (alist-get op \,-operator-function-alist op)
              (\,-group-precedence assoc left remaining-ops)
              (\,-group-precedence assoc right remaining-ops)))))))

;; By recursively applying a function that groups at the first
;; operator, we get right-associative grouping like (a ^ (b ^ c)).
;; This is why `right' is the default token direction.
(defun \,--group-operator (ops tokens)
  "Split TOKENS around the first occurence of any of the OPS.

Return a three-element list containing the tokens before the
operator, then the operator, then the tokens after the operator.

If no operator is found, the second two elements of the list will
be nil or missing."
  (-let [(left right) (--split-with (not (memq it ops)) tokens)]
    (list left (car right) (cdr right))))

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

;; Normally, backquote.el sets the docstring for `\,' using the
;; function-documentation symbol property, and sets the
;; reader-construct property, which makes `describe-function' say it
;; is a reader construct and not include a signature.
;;
;; We could just delete both of those properties and rely on the
;; documentation from the macro's definition, but
;; `help-fns--signature', doesn't handle the comma very nicely (it
;; would print as (\, EXPR)).
;;
;; Instead, we take advantage of the lack of signature in reader-
;; construct help output and add the proper signature, which looks
;; correct in output. While we're at it, add back the see-also bit
;; from backquote.el to avoid confusion, just after the summary line
;; of the `commath' docstring.
(put '\, 'function-documentation nil)

(let* ((doc (documentation '\,))
       (split (+ 2 (save-match-data (string-match "\n\n" doc)))))
  (put '\, 'function-documentation
       (concat ",EXPRESSION"
               "\n\n"
               (substring doc 0 split) ;; take first line + newline
               "(See `\\=`' (also `pcase') for other usages of `,'.)"
               "\n\n"
               (substring doc split))))

;; Local Variables:
;; lexical-binding: t;
;; read-symbol-shorthands: ((",-" . "commath-"));
;; End:
;;; commath.el ends here
