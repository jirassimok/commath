;;; commath.el --- Non-prefix math using special comma syntax -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Jacob Komissar

;; Author: Jacob Komissar <18540209+jirassimok@users.noreply.github.com>
;; Version: 1.1
;; Package-Requires: ((emacs "28.1") (dash "2.14.0"))
;; Keywords: extensions, lisp
;; Homepage: https://github.com/jirassimok/commath

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library implements a macro that allows infix math notation in
;; Emacs Lisp. The macro is named `\,' (or `commath') and it rewrites
;; certain types of expressions ("commath expressions") as Emacs Lisp
;; expressions.
;;
;; Commath supports both infix math operators and C-style function
;; calls, as well as customizable operators.
;;
;; This is meant as a convenience for writing and reading long math
;; expressions, and especially comparisons.
;;
;; See the documentation of the macro for more details.
;;
;; Examples:
;;   ,(1 + 2)  =>  (+ 1 2)
;;   ,(a < b)  =>  (< a b)
;;   ,(3 * x + 4 * y)  =>  (+ (* 3 x) (* 4 y))
;;   ,(4 * x ^ (2 * y))  =>  (* 4 (expt x (* 2 y)))
;;   ,(cos(n * pi) + 1)  =>  (+ (cos (* 2 float-pi)) 1)
;;
;; TODO:
;; - Auto-group and/or/+/* operations.
;;   - Convert subtraction to negation and addition.
;; - Add chainable comparisons.
;; - Fix operator alist to reject lambdas in customization.
;; - Better error messages.
;; - Make `describe-function' report `\,' as a macro
;; - Make group-precedence efficient again?
;; - Add a custom error type (`define-error'/`signal')
;; - Optimize `\,--wrap'? Minimal impact, but I don't like how many
;;   extra sets of parentheses are currently introduced.

;;; Code:
(require 'backquote) ;; for docstring reasons
(require 'dash)

;;;; Variable declarations and setup

(defgroup commath ()
  "Comma-syntax infix math."
  :group 'lisp
  :group 'extensions)

(defcustom \,-operator-function-alist
  '((^ . expt))
  "List associating commath operators with lisp functions.

If an operator is in this list, `commath' will expand it to the
associated function. Any operator not in this list will be
expanded to itself.

The values in this alist must be function names, not lambdas."
  :type '(alist :key-type symbol :value-type function)
  :group 'commath)

(defcustom \,-operator-rules
  '((left and)
    (left or)
    (left < > <= >= = /=)
    (left + -)
    (left * / % mod)
    (right ^))
  "List of commath operators ordered by precedence and tagged by
associativity.

This is a list of operator sets in precedence order, which each
operator set being a cons of either the symbol `left' or the
symbol `right', representing the operators' associativity, and a
list of operator symbols."
  :type '(repeat (const (choice (const 'left) (const 'right))
                        (repeat symbol)))
  :group 'commath)

;;;; Main function and macro definitions

(defmacro commath (&rest expr)
  "Rewrite math EXPRESSIONs as Lisp forms.

This macro allows infix math operations and comparisons, formed
from a limited set of expression types, refered to as \"commath
expressions.\"

There are four types of commath expression. First is a simple
value, which may be a number or variable name. These are not
rewritten by commath.

Second is X OP Y, where X and Y are commath expressions, and OP
is an infix operator from the list below. There must be spaces
around all infix operators.

Third is grouping, which may use either parentheses or brackets,
i.e. (EXPR) or [EXPR]. If the grouping symbols are left out of an
expression, it will be implicitly grouped according to standard
operator precedence and associativity, similar to C.

The final type of commath expression is a function call, of the
form NAME(ARG1, ARG2, ...). NAME is an Emacs Lisp function, and
ARG1, ARG2, and so on are commath expressions to use as its
arguments.

The standard operators are `+', `-', `*', `/', `%', `mod', `<',
`>', `<=', `>=', `/=', `and', `or', and `^'. These expand to the
Emacs Lisp functions of the same names, except for `^', which
expands to `expt'. The operators may be customzied by in
`commath-operator-rules', and their expansions can be customized
in `commath-operator-function-alist'.

Here is an example demonstrating all of these features:
    ,(1 / 2 * (a - 3 ^ [x / 4] ^ 5)
      * nth(2, list(1, 2, 3)))

That expands to the following:
    (* (* (/ 1 2)
          (- a (expt 3
                     (expt (/ x 4)
                           5))))
       (nth 2 (list 1 2 3)))

\(fn EXPRESSION)"
  (pcase (length expr)
    (1 `(\,-simple-expr ,(car expr)))
    (2 `(\,-fn-expr ,(car expr) ,(cadr expr)))
    (3 `(\,-op-expr ,(car expr) ,(cadr expr) ,(caddr expr)))
    (_ (\,-group-precedence expr \,-operator-rules))))

;; Define like `\`'; `\,' appears as its own object, not an alias.
(defalias '\, (symbol-function 'commath))

(defun \,-token-type (token)
  "Simple token checker. Performs naive token type check
and returns unprocessed token types."
  (pcase token
    ((pred numberp) 'number)
    ((pred \,-operator-p) 'operator)
    ((pred vectorp) 'vector-group)
    ((pred symbolp) 'name)
    ;; Reject other non-conses
    ((pred (not consp)) nil)
    ;; Reject quote-like conses
    ((app car (or 'quote '\, '\` '\,@)) nil)
    ;; other cons must be group or args
    (_ 'group-or-args)))

(defun \,-operator-p (symbol)
  "Return non-nil if the symbol is a `commath' operator."
  ;; (memq symbol (-mapcat 'cdr \,-operator-rules))
  (named-let recur ((ops \,-operator-rules))
    (cond ((null ops) nil)
          ((memq symbol (cdar ops)) t)
          (:else (recur (cdr ops))))))

;; Convenience function for creating commath forms in macros
(defun \,--wrap (arg)
  "Put a comma in front of something."
  (list '\, arg))

(defmacro \,-simple-expr (arg)
  "Expand single-argument commath input.

This must be a number, variable name, or group."
  (pcase (\,-token-type arg)
    ('number arg)
    ('name arg)
    ('vector-group (\,--wrap (append arg nil)))
    ;; This is the only place we can't use `\,--wrap', because wrapping
    ;; a single list arg just rewrites (, (a b)) as itself.
    ('group-or-args (cons '\, arg))
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
      (cons fname (--map (\,--wrap it) (\,-split-fn-args args))))
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

(defun \,-group-precedence (tokens remaining-ops)
  "Simplify commath expression TOKENS by grouping around an operator.

REMAINING-OPS is a list of operators in the format of
`commath-operator-rules'.

The list of TOKENS may be destructively modified."
  (when (length< tokens 4)
    ;; This function simplifies expressions with more than 3 tokens
    ;; only; if there are fewer, something else should have been used.
    (error "Invalid call to `commath-group-precedence'"))

  ;; This call will search for the first set of ops in the list.
  (-let [((assoc . ops) . later-ops) remaining-ops]
    (cond
     ;; No operators left: no grouping was possible
     ((null ops)
      (error "No operators found in commath expression (%s)." tokens))
     ;; Target operator not present: search for rest.
     ((--none? (memq it ops) tokens)
      (\,-group-precedence tokens later-ops))
     ;; Target operator is present
     (:else
      ;; For left-associative operators, we have to flip everything.
      (when (eq assoc 'left)
        (setq tokens (reverse tokens)))
      (-let [(left op right) (\,--group-operator ops tokens)]
        ;; Note: This would be more efficient with direct recursion (see
        ;; git history), but for now, rewriting in terms of simpler
        ;; expressions is more inline with our goals.
        (\,--wrap
         (list (if (eq assoc 'left) (reverse right) left)
               op
               (if (eq assoc 'left) (reverse left) right))))))))

;; By recursively applying a function that groups at the first operator,
;; we get right-associative grouping like (a ^ (b ^ c)). This is why
;; `right' is the default token direction.
(defun \,--group-operator (ops tokens)
  "Split TOKENS around the first occurence of any of the OPS.

Return a three-element list containing the tokens before the
operator, then the operator, then the tokens after the operator.

If no operator is found, signal an error."
  (-let [(left right) (--split-with (not (memq it ops)) tokens)]
    (if right
        (list left (car right) (cdr right))
      (error "No operator found for grouping."))))

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

;;;; Documentation setup

;; Normally, backquote.el sets the docstring for `\,' using the
;; function-documentation symbol property, and sets the reader-construct
;; property, which makes `describe-function' say it is a reader
;; construct and not include a signature.
;;
;; We could just delete both of those properties and rely on the
;; documentation from the macro's definition, but `help-fns--signature',
;; doesn't handle the comma very nicely (it would print as (\, EXPR)).
;;
;; Instead, we take advantage of the lack of signature in reader-
;; construct help output and add the proper signature, which looks
;; correct in output. While we're at it, add back the see-also bit from
;; backquote.el to avoid confusion, just after the summary line of the
;; `commath' docstring.
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

(provide 'commath)
;; Local Variables:
;; read-symbol-shorthands: ((",-" . "commath-"));
;; sentence-end-double-space: nil;
;; fill-column: 72;
;; End:
;;; commath.el ends here
