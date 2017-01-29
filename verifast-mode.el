;;; verifast-mode.el --- A major emacs mode for editing Verifast source code -*-lexical-binding: t-*-

;; Version: 0.0.1
;; Author: Necto
;; Url: https://github.com/necto/verifast-mode
;; Keywords: languages
;; Package-Requires: ((emacs "24.0"))

;; This file is distributed under the terms of both the MIT license and the
;; Apache License (version 2.0).

;;; Commentary:
;;

;;; Code:

(eval-when-compile (require 'rx)
                   (require 'compile)
                   (require 'url-vars))

(defvar electric-pair-inhibit-predicate)
(defvar electric-indent-chars)

;; for GNU Emacs < 24.3
(eval-when-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val))))

(defconst verifast-re-ident "[[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")
(defconst verifast-re-lc-ident "[[:lower:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")
(defconst verifast-re-uc-ident "[[:upper:]][[:word:][:multibyte:]_[:digit:]]*")
(defconst verifast-re-vis "pub")
(defconst verifast-re-extern "extern")

(defconst verifast-re-non-standard-string
  (rx
   (or
    ;; Raw string: if it matches, it ends up with the starting character
    ;; of the string as group 1, any ending backslashes as group 4, and
    ;; the ending character as either group 5 or group 6.
    (seq
     ;; The "r" starts the raw string.  Capture it as group 1 to mark it as such syntactically:
     (group "r")

     ;; Then either:
     (or
      ;; a sequence at least one "#" (followed by quote).  Capture all
      ;; but the last "#" as group 2 for this case.
      (seq (group (* "#")) "#\"")

      ;; ...or a quote without any "#".  Capture it as group 3. This is
      ;; used later to match the opposite quote only if this capture
      ;; occurred
      (group "\""))

     ;; The contents of the string:
     (*? anything)

     ;; If there are any backslashes at the end of the string, capture
     ;; them as group 4 so we can suppress the normal escape syntax
     ;; parsing:
     (group (* "\\"))

     ;; Then the end of the string--the backreferences ensure that we
     ;; only match the kind of ending that corresponds to the beginning
     ;; we had:
     (or
      ;; There were "#"s - capture the last one as group 5 to mark it as
      ;; the end of the string:
      (seq "\"" (backref 2) (group "#"))

      ;; No "#"s - capture the ending quote (using a backref to group 3,
      ;; so that we can't match a quote if we had "#"s) as group 6
      (group (backref 3))

      ;; If the raw string wasn't actually closed, go all the way to the end
      string-end))

    ;; Character literal: match the beginning ' of a character literal
    ;; as group 7, and the ending one as group 8
    (seq
     (group "'")
     (or
      (seq
       "\\"
       (or
        (: "u{" (** 1 6 xdigit) "}")
        (: "x" (= 2 xdigit))
        (any "'nrt0\"\\")))
      (not (any "'\\"))
      )
     (group "'"))
    )
   ))

(defun verifast-looking-back-str (str)
  "Like `looking-back' but for fixed strings rather than regexps (so that it's not so slow)"
  (let ((len (length str)))
    (and (> (point) len)
         (equal str (buffer-substring-no-properties (- (point) len) (point))))))

(defun verifast-looking-back-symbols (SYMS)
  "Return non-nil if the point is just after a complete symbol that is a member of the list of strings SYMS"
  (save-excursion
    (let* ((pt-orig (point))
           (beg-of-symbol (progn (forward-thing 'symbol -1) (point)))
           (end-of-symbol (progn (forward-thing 'symbol 1) (point))))
      (and
       (= end-of-symbol pt-orig)
       (member (buffer-substring-no-properties beg-of-symbol pt-orig) SYMS)))))

(defun verifast-looking-back-ident ()
  "Non-nil if we are looking backwards at a valid verifast identifier"
  (let ((beg-of-symbol (save-excursion (forward-thing 'symbol -1) (point))))
    (looking-back verifast-re-ident beg-of-symbol)))

(defun verifast-looking-back-macro ()
  "Non-nil if looking back at an ident followed by a !"
  (save-excursion (backward-char) (and (= ?! (char-after)) (verifast-looking-back-ident))))

;; Syntax definitions and helpers
(defvar verifast-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?& ?| ?^ ?! ?< ?> ?~ ?@))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Angle brackets.  We suppress this with syntactic fontification when
    ;; needed
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23n"  table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table))

(defgroup verifast-mode nil
  "Support for Verifast code."
  :link '(url-link "https://github.com/verifast/verifast")
  :group 'languages)

(defcustom verifast-indent-offset 4
  "Indent Verifast code by this number of spaces."
  :type 'integer
  :group 'verifast-mode
  :safe #'integerp)

(defcustom verifast-indent-method-chain nil
  "Indent Verifast method chains, aligned by the '.' operators"
  :type 'boolean
  :group 'verifast-mode
  :safe #'booleanp)

(defcustom verifast-indent-where-clause t
  "Indent the line starting with the where keyword following a
function or trait.  When nil, where will be aligned with fn or trait."
  :type 'boolean
  :group 'verifast-mode
  :safe #'booleanp)

(defcustom verifast-playpen-url-format "https://play.verifast-lang.org/?code=%s"
  "Format string to use when submitting code to the playpen"
  :type 'string
  :group 'verifast-mode)
(defcustom verifast-shortener-url-format "https://is.gd/create.php?format=simple&url=%s"
  "Format string to use for creating the shortened link of a playpen submission"
  :type 'string
  :group 'verifast-mode)

(defcustom verifast-match-angle-brackets t
  "Enable angle bracket matching.  Attempt to match `<' and `>' where
  appropriate."
  :type 'boolean
  :safe #'booleanp
  :group 'verifast-mode)

(defun verifast-paren-level () (nth 0 (syntax-ppss)))
(defun verifast-in-str-or-cmnt () (nth 8 (syntax-ppss)))
(defun verifast-rewind-past-str-cmnt () (goto-char (nth 8 (syntax-ppss))))

(defun verifast-rewind-irrelevant ()
  (let ((continue t))
    (while continue
      (let ((starting (point)))
        (skip-chars-backward "[:space:]\n")
        (when (verifast-looking-back-str "*/")
          (backward-char))
        (when (verifast-in-str-or-cmnt)
          (verifast-rewind-past-str-cmnt))
        ;; Rewind until the point no longer moves
        (setq continue (/= starting (point)))))))


(defun verifast-in-macro ()
  (save-excursion
    (when (> (verifast-paren-level) 0)
      (backward-up-list)
      (verifast-rewind-irrelevant)
      (or (verifast-looking-back-macro)
          (and (verifast-looking-back-ident) (save-excursion (backward-sexp) (verifast-rewind-irrelevant) (verifast-looking-back-str "macro_rules!")))
          (verifast-in-macro))
      )))

(defun verifast-looking-at-where ()
  "Return T when looking at the \"where\" keyword."
  (and (looking-at-p "\\bwhere\\b")
       (not (verifast-in-str-or-cmnt))))

(defun verifast-rewind-to-where (&optional limit)
  "Rewind the point to the closest occurrence of the \"where\" keyword.
Return T iff a where-clause was found.  Does not rewind past
LIMIT when passed, otherwise only stops at the beginning of the
buffer."
  (when (re-search-backward "\\bwhere\\b" limit t)
    (if (verifast-in-str-or-cmnt)
        (verifast-rewind-to-where limit)
      t)))

(defun verifast-align-to-expr-after-brace ()
  (save-excursion
    (forward-char)
    ;; We don't want to indent out to the open bracket if the
    ;; open bracket ends the line
    (when (not (looking-at "[[:blank:]]*\\(?://.*\\)?$"))
      (when (looking-at "[[:space:]]")
    (forward-word 1)
    (backward-word 1))
      (current-column))))

(defun verifast-rewind-to-beginning-of-current-level-expr ()
  (let ((current-level (verifast-paren-level)))
    (back-to-indentation)
    (when (looking-at "->")
      (verifast-rewind-irrelevant)
      (back-to-indentation))
    (while (> (verifast-paren-level) current-level)
      (backward-up-list)
      (back-to-indentation))
    ;; When we're in the where clause, skip over it.  First find out the start
    ;; of the function and its paren level.
    (let ((function-start nil) (function-level nil))
      (save-excursion
        (verifast-beginning-of-defun)
        (back-to-indentation)
        ;; Avoid using multiple-value-bind
        (setq function-start (point)
              function-level (verifast-paren-level)))
      ;; On a where clause
      (when (or (verifast-looking-at-where)
                ;; or in one of the following lines, e.g.
                ;; where A: Eq
                ;;       B: Hash <- on this line
                (and (save-excursion
                       (verifast-rewind-to-where function-start))
                     (= current-level function-level)))
        (goto-char function-start)))))

(defun verifast-align-to-method-chain ()
  (save-excursion
    ;; for method-chain alignment to apply, we must be looking at
    ;; another method call or field access or something like
    ;; that. This avoids rather "eager" jumps in situations like:
    ;;
    ;; {
    ;;     something.foo()
    ;; <indent>
    ;;
    ;; Without this check, we would wind up with the cursor under the
    ;; `.`. In an older version, I had the inverse of the current
    ;; check, where we checked for situations that should NOT indent,
    ;; vs checking for the one situation where we SHOULD. It should be
    ;; clear that this is more robust, but also I find it mildly less
    ;; annoying to have to press tab again to align to a method chain
    ;; than to have an over-eager indent in all other cases which must
    ;; be undone via tab.

    (when (looking-at (concat "\s*\." verifast-re-ident))
      (forward-line -1)
      (end-of-line)
      ;; Keep going up (looking for a line that could contain a method chain)
      ;; while we're in a comment or on a blank line. Stop when the paren
      ;; level changes.
      (let ((level (verifast-paren-level)))
        (while (and (or (verifast-in-str-or-cmnt)
                        ;; Only whitespace (or nothing) from the beginning to
                        ;; the end of the line.
                        (looking-back "^\s*" (point-at-bol)))
                    (= (verifast-paren-level) level))
          (forward-line -1)
          (end-of-line)))

      (let
          ;; skip-dot-identifier is used to position the point at the
          ;; `.` when looking at something like
          ;;
          ;;      foo.bar
          ;;         ^   ^
          ;;         |   |
          ;;         |  position of point
          ;;       returned offset
          ;;
          ((skip-dot-identifier
            (lambda ()
              (when (and (verifast-looking-back-ident) (save-excursion (forward-thing 'symbol -1) (= ?. (char-before))))
                (forward-thing 'symbol -1)
                (backward-char)
                (- (current-column) verifast-indent-offset)))))
        (cond
         ;; foo.bar(...)
         ((verifast-looking-back-str ")")
          (backward-list 1)
          (funcall skip-dot-identifier))

         ;; foo.bar
         (t (funcall skip-dot-identifier)))))))

(defun verifast-mode-indent-line ()
  (interactive)
  (let ((indent
         (save-excursion
           (back-to-indentation)
           ;; Point is now at beginning of current line
           (let* ((level (verifast-paren-level))
                  (baseline
                   ;; Our "baseline" is one level out from the indentation of the expression
                   ;; containing the innermost enclosing opening bracket.  That
                   ;; way if we are within a block that has a different
                   ;; indentation than this mode would give it, we still indent
                   ;; the inside of it correctly relative to the outside.
                   (if (= 0 level)
                       0
                     (or
                      (when verifast-indent-method-chain
                        (verifast-align-to-method-chain))
                      (save-excursion
                        (verifast-rewind-irrelevant)
                        (backward-up-list)
                        (verifast-rewind-to-beginning-of-current-level-expr)
                        (+ (current-column) verifast-indent-offset))))))
             (cond
              ;; Indent inside a non-raw string only if the the previous line
              ;; ends with a backslash that is inside the same string
              ((nth 3 (syntax-ppss))
               (let*
                   ((string-begin-pos (nth 8 (syntax-ppss)))
                    (end-of-prev-line-pos (when (> (line-number-at-pos) 1)
                                            (save-excursion
                                              (forward-line -1)
                                              (end-of-line)
                                              (point)))))
                 (when
                     (and
                      ;; If the string begins with an "r" it's a raw string and
                      ;; we should not change the indentation
                      (/= ?r (char-after string-begin-pos))

                      ;; If we're on the first line this will be nil and the
                      ;; rest does not apply
                      end-of-prev-line-pos

                      ;; The end of the previous line needs to be inside the
                      ;; current string...
                      (> end-of-prev-line-pos string-begin-pos)

                      ;; ...and end with a backslash
                      (= ?\\ (char-before end-of-prev-line-pos)))

                   ;; Indent to the same level as the previous line, or the
                   ;; start of the string if the previous line starts the string
                   (if (= (line-number-at-pos end-of-prev-line-pos) (line-number-at-pos string-begin-pos))
                       ;; The previous line is the start of the string.
                       ;; If the backslash is the only character after the
                       ;; string beginning, indent to the next indent
                       ;; level.  Otherwise align with the start of the string.
                       (if (> (- end-of-prev-line-pos string-begin-pos) 2)
                           (save-excursion
                             (goto-char (+ 1 string-begin-pos))
                             (current-column))
                         baseline)

                     ;; The previous line is not the start of the string, so
                     ;; match its indentation.
                     (save-excursion
                       (goto-char end-of-prev-line-pos)
                       (back-to-indentation)
                       (current-column))))))

              ;; A function return type is indented to the corresponding function arguments
              ((looking-at "->")
               (save-excursion
                 (backward-list)
                 (or (verifast-align-to-expr-after-brace)
                     (+ baseline verifast-indent-offset))))

              ;; A closing brace is 1 level unindented
              ((looking-at "[]})]") (- baseline verifast-indent-offset))

              ;; Doc comments in /** style with leading * indent to line up the *s
              ((and (nth 4 (syntax-ppss)) (looking-at "*"))
               (+ 1 baseline))

              ;; When the user chose not to indent the start of the where
              ;; clause, put it on the baseline.
              ((and (not verifast-indent-where-clause)
                    (verifast-looking-at-where))
               baseline)

              ;; If we're in any other token-tree / sexp, then:
              (t
               (or
                ;; If we are inside a pair of braces, with something after the
                ;; open brace on the same line and ending with a comma, treat
                ;; it as fields and align them.
                (when (> level 0)
                  (save-excursion
                    (verifast-rewind-irrelevant)
                    (backward-up-list)
                    ;; Point is now at the beginning of the containing set of braces
                    (verifast-align-to-expr-after-brace)))

                ;; When where-clauses are spread over multiple lines, clauses
                ;; should be aligned on the type parameters.  In this case we
                ;; take care of the second and following clauses (the ones
                ;; that don't start with "where ")
                (save-excursion
                  ;; Find the start of the function, we'll use this to limit
                  ;; our search for "where ".
                  (let ((function-start nil) (function-level nil))
                    (save-excursion
                      (verifast-beginning-of-defun)
                      (back-to-indentation)
                      ;; Avoid using multiple-value-bind
                      (setq function-start (point)
                            function-level (verifast-paren-level)))
                    ;; When we're not on a line starting with "where ", but
                    ;; still on a where-clause line, go to "where "
                    (when (and
                           (not (verifast-looking-at-where))
                           ;; We're looking at something like "F: ..."
                           (looking-at (concat verifast-re-ident ":"))
                           ;; There is a "where " somewhere after the
                           ;; start of the function.
                           (verifast-rewind-to-where function-start)
                           ;; Make sure we're not inside the function
                           ;; already (e.g. initializing a struct) by
                           ;; checking we are the same level.
                           (= function-level level))
                      ;; skip over "where"
                      (forward-char 5)
                      ;; Unless "where" is at the end of the line
                      (if (eolp)
                          ;; in this case the type parameters bounds are just
                          ;; indented once
                          (+ baseline verifast-indent-offset)
                        ;; otherwise, skip over whitespace,
                        (skip-chars-forward "[:space:]")
                        ;; get the column of the type parameter and use that
                        ;; as indentation offset
                        (current-column)))))

                (progn
                  (back-to-indentation)
                  ;; Point is now at the beginning of the current line
                  (if (or
                       ;; If this line begins with "else" or "{", stay on the
                       ;; baseline as well (we are continuing an expression,
                       ;; but the "else" or "{" should align with the beginning
                       ;; of the expression it's in.)
                       ;; Or, if this line starts a comment, stay on the
                       ;; baseline as well.
                       (looking-at "\\<else\\>\\|{\\|/[/*]")

                       (save-excursion
                         (verifast-rewind-irrelevant)
                         ;; Point is now at the end of the previous line
                         (or
                          ;; If we are at the start of the buffer, no
                          ;; indentation is needed, so stay at baseline...
                          (= (point) 1)
                          ;; ..or if the previous line ends with any of these:
                          ;;     { ? : ( , ; [ }
                          ;; then we are at the beginning of an expression, so stay on the baseline...
                          (looking-back "[(,:;?[{}]\\|[^|]|" (- (point) 2))
                          ;; or if the previous line is the end of an attribute, stay at the baseline...
                          (progn (verifast-rewind-to-beginning-of-current-level-expr) (looking-at "#")))))
                      baseline

                    ;; Otherwise, we are continuing the same expression from the previous line,
                    ;; so add one additional indent level
                    (+ baseline verifast-indent-offset))))))))))

    (when indent
      ;; If we're at the beginning of the line (before or at the current
      ;; indentation), jump with the indentation change.  Otherwise, save the
      ;; excursion so that adding the indentations will leave us at the
      ;; equivalent position within the line to where we were before.
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion (indent-line-to indent))))))


;; Font-locking definitions and helpers
(defconst verifast-mode-keywords
  '("lemma" "fixpoint" "predicate"
    "requires" "ensures" "assert"
    "switch" "case" "break"
    "return" "typedef"
    "const" "continue"
    "do"
    "else" "enum" "extern"
    "false" "for"
    "if"
    "static" "struct"
    "true"
    "while"))

(defconst verifast-special-types
  '("u8" "i8"
    "u16" "i16"
    "u32" "i32"
    "u64" "i64"

    "f32" "f64"
    "float" "int" "uint" "isize" "usize"
    "bool"
    "str" "char"))

(defconst verifast-re-type-or-constructor
  (rx symbol-start
      (group upper (0+ (any word nonascii digit "_")))
      symbol-end))

(defconst verifast-re-pre-expression-operators "[-=!%&*/:<>[{(|.^;}]")
(defun verifast-re-word (inner) (concat "\\<" inner "\\>"))
(defun verifast-re-grab (inner) (concat "\\(" inner "\\)"))
(defun verifast-re-shy (inner) (concat "\\(?:" inner "\\)"))
(defun verifast-re-item-def (itype)
  (concat (verifast-re-word itype) "[[:space:]]+" (verifast-re-grab verifast-re-ident)))
(defun verifast-re-item-def-imenu (itype)
  (concat "^[[:space:]]*"
          (verifast-re-shy (concat (verifast-re-word verifast-re-vis) "[[:space:]]+")) "?"
          (verifast-re-shy (concat (verifast-re-word verifast-re-extern) "[[:space:]]+"
                               (verifast-re-shy "\"[^\"]+\"[[:space:]]+") "?")) "?"
          (verifast-re-item-def itype)))

(defconst verifast-re-special-types (regexp-opt verifast-special-types 'symbols))


(defun verifast-path-font-lock-matcher (re-ident)
  "Matches names like \"foo::\" or \"Foo::\" (depending on RE-IDENT, which should match
the desired identifiers), but does not match type annotations \"foo::<\"."
  `(lambda (limit)
     (catch 'verifast-path-font-lock-matcher
       (while t
         (let* ((symbol-then-colons (rx-to-string '(seq (group (regexp ,re-ident)) "::")))
                (match (re-search-forward symbol-then-colons limit t)))
           (cond
            ;; If we didn't find a match, there are no more occurrences
            ;; of foo::, so return.
            ((null match) (throw 'verifast-path-font-lock-matcher nil))
            ;; If this isn't a type annotation foo::<, we've found a
            ;; match, so a return it!
            ((not (looking-at (rx (0+ space) "<")))
	     (throw 'verifast-path-font-lock-matcher match))))))))

(defvar verifast-mode-font-lock-keywords
  (append
   `(
     ;; Keywords proper
     (,(regexp-opt verifast-mode-keywords 'symbols) . font-lock-keyword-face)

     ;; Special types
     (,(regexp-opt verifast-special-types 'symbols) . font-lock-type-face)

     ;; Attributes like `#[bar(baz)]` or `#![bar(baz)]` or `#[bar = "baz"]`
     (,(verifast-re-grab (concat "#\\!?\\[" verifast-re-ident "[^]]*\\]"))
      1 font-lock-preprocessor-face keep)

     ;; Syntax extension invocations like `foo!`, highlight including the !
     (,(concat (verifast-re-grab (concat verifast-re-ident "!")) "[({[:space:][]")
      1 font-lock-preprocessor-face)

     ;; Field names like `foo:`, highlight excluding the :
     (,(concat (verifast-re-grab verifast-re-ident) ":[^:]") 1 font-lock-variable-name-face)

     ;; Type names like `Foo::`, highlight excluding the ::
     (,(verifast-path-font-lock-matcher verifast-re-uc-ident) 1 font-lock-type-face)

     ;; Module names like `foo::`, highlight excluding the ::
     (,(verifast-path-font-lock-matcher verifast-re-lc-ident) 1 font-lock-constant-face)

     ;; Lifetimes like `'foo`
     (,(concat "'" (verifast-re-grab verifast-re-ident) "[^']") 1 font-lock-variable-name-face)

     ;; CamelCase Means Type Or Constructor
     (,verifast-re-type-or-constructor 1 font-lock-type-face)
     )

   ;; Item definitions
   (mapcar #'(lambda (x)
               (list (verifast-re-item-def (car x))
                     1 (cdr x)))
           '(("enum" . font-lock-type-face)
             ("struct" . font-lock-type-face)
             ("type" . font-lock-type-face)
             ("mod" . font-lock-constant-face)
             ("use" . font-lock-constant-face)
             ("fn" . font-lock-function-name-face)))))

(defvar font-lock-beg)
(defvar font-lock-end)

(defun verifast-font-lock-extend-region ()
  "Extend the region given by `font-lock-beg' and `font-lock-end'
  to include the beginning of a string or comment if it includes
  part of it.  Adjusts to include the r[#] of a raw string as
  well."

  (save-excursion
    (let ((orig-beg font-lock-beg)
          (orig-end font-lock-end))

      (let*
          ;; It's safe to call `syntax-ppss' here on positions that are
          ;; already syntactically fontified
          ((beg-ppss (syntax-ppss font-lock-beg))
           (beg-in-cmnt (and beg-ppss (nth 4 beg-ppss) (nth 8 beg-ppss)))
           (beg-in-str (and beg-ppss (nth 3 beg-ppss) (nth 8 beg-ppss))))

        (when (and beg-in-str (>= font-lock-beg beg-in-str))
          (setq font-lock-beg (nth 8 beg-ppss))
          (while (equal ?# (char-before font-lock-beg))
            (setq font-lock-beg (1- font-lock-beg)))
          (when (equal ?r (char-before font-lock-beg))
            (setq font-lock-beg (1- font-lock-beg))))

        (when (and beg-in-cmnt (> font-lock-beg beg-in-cmnt))
          (setq font-lock-beg beg-in-cmnt)))

      ;; We need to make sure that if the region ends inside a raw string, we
      ;; extend it out past the end of it.  But we can't use `syntax-ppss' to
      ;; detect that, becaue that depends on font-lock already being done, and we
      ;; are trying to figure out how much to font-lock before that.  So we use
      ;; the regexp directly.
      (save-match-data
        (goto-char font-lock-beg)
        (while (and (< (point) font-lock-end)
                    (re-search-forward verifast-re-non-standard-string (buffer-end 1) t)
                    (<= (match-beginning 0) font-lock-end))
          (setq font-lock-end (max font-lock-end (match-end 0)))
          (goto-char (1+ (match-beginning 0)))))

      (or (/= font-lock-beg orig-beg)
          (/= font-lock-end orig-end))
      )))

(defun verifast-conditional-re-search-forward (regexp bound condition)
  ;; Search forward for regexp (with bound).  If found, call condition and return the found
  ;; match only if it returns true.
  (let* (found
         found-ret-list
         (ret-list (save-excursion
                     (while (and (not found) (re-search-forward regexp bound t))
                       (setq
                        found-ret-list (list (point) (match-data))
                        found (save-match-data (save-excursion (ignore-errors (funcall condition)))))
                       ;; If the condition filters out a match, need to search
                       ;; again just after its beginning.  This will allow
                       ;; cases such as:
                       ;;    "bar" r"foo"
                       ;; where the filtered out search (r" r") should not
                       ;; prevent finding another one that begins in the middle
                       ;; of it (r"foo")
                       (when (not found)
                         (goto-char (1+ (match-beginning 0))))
                       )
                     (when found found-ret-list))))
    (when ret-list
      (goto-char (nth 0 ret-list))
      (set-match-data (nth 1 ret-list))
      (nth 0 ret-list))))

(defun verifast-look-for-non-standard-string (bound)
  ;; Find a raw string or character literal, but only if it's not in the middle
  ;; of another string or a comment.

  (verifast-conditional-re-search-forward
   verifast-re-non-standard-string
   bound
   (lambda ()
     (let ((pstate (syntax-ppss (match-beginning 0))))
       (not
        (or
         (nth 4 pstate) ;; Skip if in a comment
         (and (nth 3 pstate) (wholenump (nth 8 pstate)) (< (nth 8 pstate) (match-beginning 0))) ;; Skip if in a string that isn't starting here
         ))))))

(defun verifast-syntax-class-before-point ()
  (when (> (point) 1)
    (syntax-class (syntax-after (1- (point))))))

(defun verifast-rewind-qualified-ident ()
  (while (verifast-looking-back-ident)
    (backward-sexp)
    (when (save-excursion (verifast-rewind-irrelevant) (verifast-looking-back-str "::"))
      (verifast-rewind-irrelevant)
      (backward-char 2)
      (verifast-rewind-irrelevant))))

(defun verifast-rewind-type-param-list ()
  (cond
   ((and (verifast-looking-back-str ">") (equal 5 (verifast-syntax-class-before-point)))
    (backward-sexp)
    (verifast-rewind-irrelevant))

   ;; We need to be able to back up past the Fn(args) -> RT form as well.  If
   ;; we're looking back at this, we want to end up just after "Fn".
   ((member (char-before) '(?\] ?\) ))
    (let* ((is-paren (verifast-looking-back-str ")"))
           (dest (save-excursion
                  (backward-sexp)
                  (verifast-rewind-irrelevant)
                  (or
                   (when (verifast-looking-back-str "->")
                     (backward-char 2)
                     (verifast-rewind-irrelevant)
                     (when (verifast-looking-back-str ")")
                       (backward-sexp)
                       (point)))
                   (and is-paren (point))))))
      (when dest
        (goto-char dest))))))

(defun verifast-rewind-to-decl-name ()
  "If we are before an ident that is part of a declaration that
  can have a where clause, rewind back to just before the name of
  the subject of that where clause and return the new point.
  Otherwise return nil"
  
  (let* ((ident-pos (point))
         (newpos (save-excursion
                   (verifast-rewind-irrelevant)
                   (verifast-rewind-type-param-list)
                   (cond
                       ((verifast-looking-back-symbols '("fn" "trait" "enum" "struct" "impl" "type")) ident-pos)

                       ((equal 5 (verifast-syntax-class-before-point))
                        (backward-sexp)
                        (verifast-rewind-to-decl-name))

                       ((looking-back "[:,'+=]" (1- (point)))
                        (backward-char)
                        (verifast-rewind-to-decl-name))

                       ((verifast-looking-back-str "->")
                        (backward-char 2)
                        (verifast-rewind-to-decl-name))

                       ((verifast-looking-back-ident)
                        (verifast-rewind-qualified-ident)
                        (verifast-rewind-to-decl-name))))))
    (when newpos (goto-char newpos))
    newpos))

(defun verifast-is-in-expression-context (token)
  "Return t if what comes right after the point is part of an
  expression (as opposed to starting a type) by looking at what
  comes before.  Takes a symbol that roughly indicates what is
  after the point.

  This function is used as part of `verifast-is-lt-char-operator' as
  part of angle bracket matching, and is not intended to be used
  outside of this context."

  (save-excursion
    (let ((postchar (char-after)))
      (verifast-rewind-irrelevant)

      ;; A type alias or ascription could have a type param list.  Skip backwards past it.
      (when (member token '(ambiguous-operator open-brace))
        (verifast-rewind-type-param-list))
      
      (cond

       ;; Certain keywords always introduce expressions
       ((verifast-looking-back-symbols '("if" "while" "match" "return" "box" "in")) t)

       ;; "as" introduces a type
       ((verifast-looking-back-symbols '("as")) nil)

       ;; An open angle bracket never introduces expression context WITHIN the angle brackets
       ((and (equal token 'open-brace) (equal postchar ?<)) nil)

       ;; An ident! followed by an open brace is a macro invocation.  Consider
       ;; it to be an expression.
       ((and (equal token 'open-brace) (verifast-looking-back-macro)) t)
       
       ;; An identifier is right after an ending paren, bracket, angle bracket
       ;; or curly brace.  It's a type if the last sexp was a type.
       ((and (equal token 'ident) (equal 5 (verifast-syntax-class-before-point)))
        (backward-sexp)
        (verifast-is-in-expression-context 'open-brace))

       ;; If a "for" appears without a ; or { before it, it's part of an
       ;; "impl X for y", so the y is a type.  Otherwise it's
       ;; introducing a loop, so the y is an expression
       ((and (equal token 'ident) (verifast-looking-back-symbols '("for")))
        (backward-sexp)
        (verifast-rewind-irrelevant)
        (looking-back "[{;]" (1- (point))))
       
       ((verifast-looking-back-ident)
        (verifast-rewind-qualified-ident)
        (verifast-rewind-irrelevant)
        (cond
         ((equal token 'open-brace)
          ;; We now know we have:
          ;;   ident <maybe type params> [{([]
          ;; where [{([] denotes either a {, ( or [.  This character is bound as postchar.
          (cond
           ;; If postchar is a paren or square bracket, then if the brace is a type if the identifier is one
           ((member postchar '(?\( ?\[ )) (verifast-is-in-expression-context 'ident))

           ;; If postchar is a curly brace, the brace can only be a type if
           ;; ident2 is the name of an enum, struct or trait being declared.
           ;; Note that if there is a -> before the ident then the ident would
           ;; be a type but the { is not.
           ((equal ?{ postchar)
            (not (and (verifast-rewind-to-decl-name)
                      (progn
                        (verifast-rewind-irrelevant)
                        (verifast-looking-back-symbols '("enum" "struct" "trait" "type"))))))
           ))
         
         ((equal token 'ambiguous-operator)
          (cond
           ;; An ampersand after an ident has to be an operator rather than a & at the beginning of a ref type
           ((equal postchar ?&) t)

           ;; A : followed by a type then an = introduces an expression (unless it is part of a where clause of a "type" declaration)
           ((and (equal postchar ?=)
                 (looking-back "[^:]:" (- (point) 2))
                 (not (save-excursion (and (verifast-rewind-to-decl-name) (progn (verifast-rewind-irrelevant) (verifast-looking-back-symbols '("type"))))))))

           ;; "let ident =" introduces an expression--and so does "const" and "mut"
           ((and (equal postchar ?=) (verifast-looking-back-symbols '("let" "const" "mut"))) t)

           ;; As a specific special case, see if this is the = in this situation:
           ;;     enum EnumName<type params> { Ident =
           ;; In this case, this is a c-like enum and despite Ident
           ;; representing a type, what comes after the = is an expression
           ((and
             (> (verifast-paren-level) 0)
             (save-excursion
               (backward-up-list)
               (verifast-rewind-irrelevant)
               (verifast-rewind-type-param-list)
               (and
                (verifast-looking-back-ident)
                (progn
                  (verifast-rewind-qualified-ident)
                  (verifast-rewind-irrelevant)
                  (verifast-looking-back-str "enum")))))
            t)
           
           ;; Otherwise the ambiguous operator is a type if the identifier is a type
           ((verifast-is-in-expression-context 'ident) t)))

         ((equal token 'colon)
          (cond
           ;; If we see a ident: not inside any braces/parens, we're at top level.
           ;; There are no allowed expressions after colons there, just types.
           ((<= (verifast-paren-level) 0) nil)

           ;; We see ident: inside a list
           ((looking-back "[{,]" (1- (point)))
            (backward-up-list)

            ;; If a : appears whose surrounding paren/brackets/braces are
            ;; anything other than curly braces, it can't be a field
            ;; initializer and must be denoting a type.
            (when (looking-at "{")
              (verifast-rewind-irrelevant)
              (verifast-rewind-type-param-list)
              (when (verifast-looking-back-ident)
                ;; We have a context that looks like this:
                ;;    ident2 <maybe type params> { [maybe paren-balanced code ending in comma] ident1:
                ;; the point is sitting just after ident2, and we trying to
                ;; figure out if the colon introduces an expression or a type.
                ;; The answer is that ident1 is a field name, and what comes
                ;; after the colon is an expression, if ident2 is an
                ;; expression.
                (verifast-rewind-qualified-ident)
                (verifast-is-in-expression-context 'ident))))


           ;; Otherwise, if the ident: appeared with anything other than , or {
           ;; before it, it can't be part of a struct initializer and therefore
           ;; must be denoting a type.
	   (t nil)
           ))
         ))

       ;; An operator-like character after a string is indeed an operator
       ((and (equal token 'ambiguous-operator)
             (member (verifast-syntax-class-before-point) '(5 7 15))) t)

       ;; A colon that has something other than an identifier before it is a
       ;; type ascription
       ((equal token 'colon) nil)

       ;; A :: introduces a type (or module, but not an expression in any case)
       ((verifast-looking-back-str "::") nil)
       
       ((verifast-looking-back-str ":")
        (backward-char)
        (verifast-is-in-expression-context 'colon))

       ;; A -> introduces a type
       ((verifast-looking-back-str "->") nil)

       ;; If we are up against the beginning of a list, or after a comma inside
       ;; of one, back up out of it and check what the list itself is
       ((or
         (equal 4 (verifast-syntax-class-before-point))
         (verifast-looking-back-str ","))
        (backward-up-list)
        (verifast-is-in-expression-context 'open-brace))

       ;; A => introduces an expression
       ((verifast-looking-back-str "=>") t)

       ;; A == introduces an expression
       ((verifast-looking-back-str "==") t)

       ;; These operators can introduce expressions or types
       ((looking-back "[-+=!?&*]" (1- (point)))
        (backward-char)
        (verifast-is-in-expression-context 'ambiguous-operator))

       ;; These operators always introduce expressions.  (Note that if this
       ;; regexp finds a < it must not be an angle bracket, or it'd
       ;; have been caught in the syntax-class check above instead of this.)
       ((looking-back verifast-re-pre-expression-operators (1- (point))) t)
       ))))

(defun verifast-is-lt-char-operator ()
  "Return t if the < sign just after point is an operator rather
  than an opening angle bracket, otherwise nil."
  
  (let ((case-fold-search nil))
    (save-excursion
      (verifast-rewind-irrelevant)
      ;; We are now just after the character syntactically before the <.
      (cond

       ;; If we are looking back at a < that is not an angle bracket (but not
       ;; two of them) then this is the second < in a bit shift operator
       ((and (verifast-looking-back-str "<")
             (not (equal 4 (verifast-syntax-class-before-point)))
             (not (verifast-looking-back-str "<<"))))
       
       ;; On the other hand, if we are after a closing paren/brace/bracket it
       ;; can only be an operator, not an angle bracket.  Likewise, if we are
       ;; after a string it's an operator.  (The string case could actually be
       ;; valid in verifast for character literals.)
       ((member (verifast-syntax-class-before-point) '(5 7 15)) t)

       ;; If we are looking back at an operator, we know that we are at
       ;; the beginning of an expression, and thus it has to be an angle
       ;; bracket (starting a "<Type as Trait>::" construct.)
       ((looking-back verifast-re-pre-expression-operators (1- (point))) nil)

       ;; If we are looking back at a keyword, it's an angle bracket
       ;; unless that keyword is "self", "true" or "false"
       ((verifast-looking-back-symbols verifast-mode-keywords)
        (verifast-looking-back-symbols '("self" "true" "false")))

       ;; If we're looking back at an identifier, this depends on whether
       ;; the identifier is part of an expression or a type
       ((verifast-looking-back-ident)
        (backward-sexp)
        (or
         ;; The special types can't take type param lists, so a < after one is
         ;; always an operator
         (looking-at verifast-re-special-types)
         
         (verifast-is-in-expression-context 'ident)))

       ;; Otherwise, assume it's an angle bracket
       ))))

(defun verifast-electric-pair-inhibit-predicate-wrap (char)
  "Wraps the default `electric-pair-inhibit-predicate' to prevent
  inserting a \"matching\" > after a < that would be treated as a
  less than sign rather than as an opening angle bracket."
  (or
   (when (= ?< char)
     (save-excursion
       (backward-char)
       (verifast-is-lt-char-operator)))
   (funcall (default-value 'electric-pair-inhibit-predicate) char)))

(defun verifast-look-for-non-angle-bracket-lt-gt (bound)
  "Find an angle bracket (\"<\" or \">\") that should be part of
  a matched pair Relies on the fact that when it finds a < or >,
  we have already decided which previous ones are angle brackets
  and which ones are not.  So this only really works as a
  font-lock-syntactic-keywords matcher--it won't work at
  arbitrary positions without the earlier parts of the buffer
  having already been covered."

  (verifast-conditional-re-search-forward
   "[<>]" bound
   (lambda ()
     (goto-char (match-beginning 0))
     (cond
      ;; If matching is turned off suppress all of them
      ((not verifast-match-angle-brackets) t)

      ;; We don't take < or > in strings or comments to be angle brackets
      ((verifast-in-str-or-cmnt) t)

      ;; Inside a macro we don't really know the syntax.  Any < or > may be an
      ;; angle bracket or it may not.  But we know that the other braces have
      ;; to balance regardless of the < and >, so if we don't treat any < or >
      ;; as angle brackets it won't mess up any paren balancing.
      ((verifast-in-macro) t)
      
      ((looking-at "<")
       (verifast-is-lt-char-operator))

      ((looking-at ">")
       (cond
        ;; Don't treat the > in -> or => as an angle bracket
        ((member (char-before (point)) '(?- ?=)) t)

        ;; If we are at top level and not in any list, it can't be a closing
        ;; angle bracket
        ((>= 0 (verifast-paren-level)) t)

        ;; Otherwise, treat the > as a closing angle bracket if it would
        ;; match an opening one
        ((save-excursion
           (backward-up-list)
           (not (looking-at "<"))))))))))

(defvar verifast-mode-font-lock-syntactic-keywords
  (append
   ;; Handle raw strings and character literals:
   `((verifast-look-for-non-standard-string (1 "|" nil t) (4 "_" nil t) (5 "|" nil t) (6 "|" nil t) (7 "\"" nil t) (8 "\"" nil t)))
   ;; Find where < and > characters represent operators rather than angle brackets:
   '((verifast-look-for-non-angle-bracket-lt-gt (0 "." t)))))

(defun verifast-mode-syntactic-face-function (state)
  "Syntactic face function to distinguish doc comments from other comments."
  (if (nth 3 state) 'font-lock-string-face
    (save-excursion
      (goto-char (nth 8 state))
      (if (looking-at "/\\([*][*!][^*!]\\|/[/!][^/!]\\)")
          'font-lock-doc-face
        'font-lock-comment-face
    ))))

(defun verifast-fill-prefix-for-comment-start (line-start)
  "Determine what to use for `fill-prefix' based on what is at the beginning of a line."
  (let ((result
         ;; Replace /* with same number of spaces
         (replace-regexp-in-string
          "\\(?:/\\*+?\\)[!*]?"
          (lambda (s)
            ;; We want the * to line up with the first * of the
            ;; comment start
            (let ((offset (if (eq t
                                  (compare-strings "/*" nil nil
                                                   s
                                                   (- (length s) 2)
                                                   (length s)))
                              1 2)))
              (concat (make-string (- (length s) offset)
                                   ?\x20) "*")))
          line-start)))
    ;; Make sure we've got at least one space at the end
    (if (not (= (aref result (- (length result) 1)) ?\x20))
        (setq result (concat result " ")))
    result))

(defun verifast-in-comment-paragraph (body)
  ;; We might move the point to fill the next comment, but we don't want it
  ;; seeming to jump around on the user
  (save-excursion
    ;; If we're outside of a comment, with only whitespace and then a comment
    ;; in front, jump to the comment and prepare to fill it.
    (when (not (nth 4 (syntax-ppss)))
      (beginning-of-line)
      (when (looking-at (concat "[[:space:]\n]*" comment-start-skip))
        (goto-char (match-end 0))))

    ;; We need this when we're moving the point around and then checking syntax
    ;; while doing paragraph fills, because the cache it uses isn't always
    ;; invalidated during this.
    (syntax-ppss-flush-cache 1)
    ;; If we're at the beginning of a comment paragraph with nothing but
    ;; whitespace til the next line, jump to the next line so that we use the
    ;; existing prefix to figure out what the new prefix should be, rather than
    ;; inferring it from the comment start.
    (let ((next-bol (line-beginning-position 2)))
      (while (save-excursion
               (end-of-line)
               (syntax-ppss-flush-cache 1)
               (and (nth 4 (syntax-ppss))
                    (save-excursion
                      (beginning-of-line)
                      (looking-at paragraph-start))
                    (looking-at "[[:space:]]*$")
                    (nth 4 (syntax-ppss next-bol))))
        (goto-char next-bol)))

    (syntax-ppss-flush-cache 1)
    ;; If we're on the last line of a multiline-style comment that started
    ;; above, back up one line so we don't mistake the * of the */ that ends
    ;; the comment for a prefix.
    (when (save-excursion
            (and (nth 4 (syntax-ppss (line-beginning-position 1)))
                 (looking-at "[[:space:]]*\\*/")))
      (goto-char (line-end-position 0)))
    (funcall body)))

(defun verifast-with-comment-fill-prefix (body)
  (let*
      ((line-string (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position)))
       (line-comment-start
        (when (nth 4 (syntax-ppss))
          (cond
           ;; If we're inside the comment and see a * prefix, use it
           ((string-match "^\\([[:space:]]*\\*+[[:space:]]*\\)"
                          line-string)
            (match-string 1 line-string))
           ;; If we're at the start of a comment, figure out what prefix
           ;; to use for the subsequent lines after it
           ((string-match (concat "[[:space:]]*" comment-start-skip) line-string)
            (verifast-fill-prefix-for-comment-start
             (match-string 0 line-string))))))
       (fill-prefix
        (or line-comment-start
            fill-prefix)))
    (funcall body)))

(defun verifast-find-fill-prefix ()
  (verifast-in-comment-paragraph (lambda () (verifast-with-comment-fill-prefix (lambda () fill-prefix)))))

(defun verifast-fill-paragraph (&rest args)
  "Special wrapping for `fill-paragraph' to handle multi-line comments with a * prefix on each line."
  (verifast-in-comment-paragraph
   (lambda ()
     (verifast-with-comment-fill-prefix
      (lambda ()
        (let
            ((fill-paragraph-function
              (if (not (eq fill-paragraph-function 'verifast-fill-paragraph))
                  fill-paragraph-function))
             (fill-paragraph-handle-comment t))
          (apply 'fill-paragraph args)
          t))))))

(defun verifast-do-auto-fill (&rest args)
  "Special wrapping for `do-auto-fill' to handle multi-line comments with a * prefix on each line."
  (verifast-with-comment-fill-prefix
   (lambda ()
     (apply 'do-auto-fill args)
     t)))

(defun verifast-fill-forward-paragraph (arg)
  ;; This is to work around some funny behavior when a paragraph separator is
  ;; at the very top of the file and there is a fill prefix.
  (let ((fill-prefix nil)) (forward-paragraph arg)))

(defun verifast-comment-indent-new-line (&optional arg)
  (verifast-with-comment-fill-prefix
   (lambda () (comment-indent-new-line arg))))

;;; Imenu support
(defvar verifast-imenu-generic-expression
  (append (mapcar #'(lambda (x)
                      (list (capitalize x) (verifast-re-item-def-imenu x) 1))
                  '("enum" "struct" "type" "mod" "fn" "trait" "impl"))
          `(("Macro" ,(verifast-re-item-def-imenu "macro_rules!") 1)))
  "Value for `imenu-generic-expression' in Verifast mode.

Create a hierarchical index of the item definitions in a Verifast file.

Imenu will show all the enums, structs, etc. in their own subheading.
Use idomenu (imenu with `ido-mode') for best mileage.")

;;; Defun Motions

;;; Start of a Verifast item
(defvar verifast-top-item-beg-re
  (concat "^\\s-*\\(?:priv\\|pub\\)?\\s-*"
          (regexp-opt
           '("enum" "struct" "type" "mod" "use" "fn" "static" "impl"
             "extern" "trait"))))

(defun verifast-beginning-of-defun (&optional arg)
  "Move backward to the beginning of the current defun.

With ARG, move backward multiple defuns.  Negative ARG means
move forward.

This is written mainly to be used as `beginning-of-defun-function' for Verifast.
Don't move to the beginning of the line. `beginning-of-defun',
which calls this, does that afterwards."
  (interactive "p")
  (re-search-backward (concat "^\\(" verifast-top-item-beg-re "\\)\\_>")
                      nil 'move (or arg 1)))

(defun verifast-end-of-defun ()
  "Move forward to the next end of defun.

With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

Assume that this is called after beginning-of-defun. So point is
at the beginning of the defun body.

This is written mainly to be used as `end-of-defun-function' for Verifast."
  (interactive)
  ;; Find the opening brace
  (if (re-search-forward "[{]" nil t)
      (progn
        (goto-char (match-beginning 0))
        ;; Go to the closing brace
        (condition-case nil
            (forward-sexp)
          (scan-error
           ;; The parentheses are unbalanced; instead of being unable to fontify, just jump to the end of the buffer
           (goto-char (point-max)))))
    ;; There is no opening brace, so consider the whole buffer to be one "defun"
    (goto-char (point-max))))

(defconst verifast--format-word "\\b\\(else\\|enum\\|fn\\|for\\|if\\|let\\|loop\\|match\\|struct\\|while\\)\\b")
(defconst verifast--format-line "\\([\n]\\)")

;; Counts number of matches of regex beginning up to max-beginning,
;; leaving the point at the beginning of the last match.
(defun verifast--format-count (regex max-beginning)
  (let ((count 0)
        save-point
        beginning)
    (while (and (< (point) max-beginning)
                (re-search-forward regex max-beginning t))
      (setq count (1+ count))
      (setq beginning (match-beginning 1)))
    ;; try one more in case max-beginning lies in the middle of a match
    (setq save-point (point))
    (when (re-search-forward regex nil t)
      (let ((try-beginning (match-beginning 1)))
        (if (> try-beginning max-beginning)
            (goto-char save-point)
          (setq count (1+ count))
          (setq beginning try-beginning))))
    (when beginning (goto-char beginning))
    count))

;; Gets list describing pos or (point).
;; The list contains:
;; 1. the number of matches of verifast--format-word,
;; 2. the number of matches of verifast--format-line after that,
;; 3. the number of columns after that.
(defun verifast--format-get-loc (buffer &optional pos)
  (with-current-buffer buffer
    (save-excursion
      (let ((pos (or pos (point)))
            words lines columns)
        (goto-char (point-min))
        (setq words (verifast--format-count verifast--format-word pos))
        (setq lines (verifast--format-count verifast--format-line pos))
        (if (> lines 0)
            (if (= (point) pos)
                (setq columns -1)
              (forward-char 1)
              (goto-char pos)
              (setq columns (current-column)))
          (let ((initial-column (current-column)))
            (goto-char pos)
            (setq columns (- (current-column) initial-column))))
        (list words lines columns)))))

;; Moves the point forward by count matches of regex up to max-pos,
;; and returns new max-pos making sure final position does not include another match.
(defun verifast--format-forward (regex count max-pos)
  (when (< (point) max-pos)
    (let ((beginning (point)))
      (while (> count 0)
        (setq count (1- count))
        (re-search-forward regex nil t)
        (setq beginning (match-beginning 1)))
      (when (re-search-forward regex nil t)
        (setq max-pos (min max-pos (match-beginning 1))))
      (goto-char beginning)))
  max-pos)

;; Gets the position from a location list obtained using verifast--format-get-loc.
(defun verifast--format-get-pos (buffer loc)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((max-pos (point-max))
            (words (pop loc))
            (lines (pop loc))
            (columns (pop loc)))
        (setq max-pos (verifast--format-forward verifast--format-word words max-pos))
        (setq max-pos (verifast--format-forward verifast--format-line lines max-pos))
        (when (> lines 0) (forward-char))
        (let ((initial-column (current-column))
              (save-point (point)))
          (move-end-of-line nil)
          (when (> (current-column) (+ initial-column columns))
            (goto-char save-point)
            (forward-char columns)))
        (min (point) max-pos)))))

(defvar verifast-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") 'verifast-format-buffer)
    map)
  "Keymap for Verifast major mode.")

;;;###autoload
(define-derived-mode verifast-mode prog-mode "VeriFast"
  "Major mode for Verifast code.

\\{verifast-mode-map}"
  :group 'verifast-mode
  :syntax-table verifast-mode-syntax-table

  ;; Indentation
  (setq-local indent-line-function 'verifast-mode-indent-line)

  ;; Fonts
  (add-to-list 'font-lock-extend-region-functions 'verifast-font-lock-extend-region)
  (setq-local font-lock-defaults '(verifast-mode-font-lock-keywords
                                   nil nil nil nil
                                   (font-lock-syntactic-keywords . verifast-mode-font-lock-syntactic-keywords)
                                   (font-lock-syntactic-face-function . verifast-mode-syntactic-face-function)
                                   ))

  ;; Misc
  (setq-local comment-start "// ")
  (setq-local comment-end   "")
  (setq-local indent-tabs-mode nil)

  ;; Auto indent on }
  (setq-local
   electric-indent-chars (cons ?} (and (boundp 'electric-indent-chars)
                                       electric-indent-chars)))

  ;; Allow paragraph fills for comments
  (setq-local comment-start-skip "\\(?://[/!]*\\|/\\*[*!]?\\)[[:space:]]*")
  (setq-local paragraph-start
       (concat "[[:space:]]*\\(?:" comment-start-skip "\\|\\*/?[[:space:]]*\\|\\)$"))
  (setq-local paragraph-separate paragraph-start)
  (setq-local normal-auto-fill-function 'verifast-do-auto-fill)
  (setq-local fill-paragraph-function 'verifast-fill-paragraph)
  (setq-local fill-forward-paragraph-function 'verifast-fill-forward-paragraph)
  (setq-local adaptive-fill-function 'verifast-find-fill-prefix)
  (setq-local adaptive-fill-first-line-regexp "")
  (setq-local comment-multi-line t)
  (setq-local comment-line-break-function 'verifast-comment-indent-new-line)
  (setq-local imenu-generic-expression verifast-imenu-generic-expression)
  (setq-local imenu-syntax-alist '((?! . "w"))) ; For macro_rules!
  (setq-local beginning-of-defun-function 'verifast-beginning-of-defun)
  (setq-local end-of-defun-function 'verifast-end-of-defun)
  (setq-local parse-sexp-lookup-properties t)
  (setq-local electric-pair-inhibit-predicate 'verifast-electric-pair-inhibit-predicate-wrap)
  (add-hook 'after-revert-hook 'verifast--after-revert-hook nil t)
  (add-hook 'before-save-hook 'verifast--before-save-hook nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rs\\'" . verifast-mode))

(defun verifast-mode-reload ()
  (interactive)
  (unload-feature 'verifast-mode)
  (require 'verifast-mode)
  (verifast-mode))

;; Issue #104: When reverting the buffer, make sure all fontification is redone
;; so that we don't end up missing a non-angle-bracket '<' or '>' character.
(defun verifast--after-revert-hook ()
  ;; In Emacs 25 and later, the preferred method to force fontification is
  ;; to use `font-lock-ensure', which doesn't exist in Emacs 24 and earlier.
  ;; If it's not available, fall back to calling `font-lock-fontify-region'
  ;; on the whole buffer.
  (save-excursion
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (font-lock-fontify-region (point-min) (point-max)))))

;; Issue #6887: Rather than inheriting the 'gnu compilation error
;; regexp (which is broken on a few edge cases), add our own 'verifast
;; compilation error regexp and use it instead.
(defvar verifastc-compilation-regexps
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)")
        (end-line   "\\([0-9]+\\)")
        (end-col    "\\([0-9]+\\)")
        (msg-type   "\\(?:[Ee]rror\\|\\([Ww]arning\\)\\|\\([Nn]ote\\|[Hh]elp\\)\\)"))
    (let ((re (concat "^" file ":" start-line ":" start-col
                      ": " end-line ":" end-col
                      " " msg-type ":")))
      (cons re '(1 (2 . 4) (3 . 5) (6 . 7)))))
  "Specifications for matching errors in verifastc invocations.
See `compilation-error-regexp-alist' for help on their format.")

(defvar verifastc-new-compilation-regexps
  (let ((file "\\([^\n]+\\)")
        (start-line "\\([0-9]+\\)")
        (start-col  "\\([0-9]+\\)"))
    (let ((re (concat "^ *--> " file ":" start-line ":" start-col ; --> 1:2:3
                      )))
      (cons re '(1 2 3))))
  "Specifications for matching errors in verifastc invocations (new style).
See `compilation-error-regexp-alist' for help on their format.")

;; Match test run failures and panics during compilation as
;; compilation warnings
(defvar cargo-compilation-regexps
  '("^\\s-+thread '[^']+' panicked at \\('[^']+', \\([^:]+\\):\\([0-9]+\\)\\)" 2 3 nil nil 1)
  "Specifications for matching panics in cargo test invocations.
See `compilation-error-regexp-alist' for help on their format.")

(defun verifastc-scroll-down-after-next-error ()
  "In the new style error messages, the regular expression
   matches on the file name (which appears after `-->`), but the
   start of the error appears a few lines earlier. This hook runs
   after `M-x next-error`; it simply scrolls down a few lines in
   the compilation window until the top of the error is visible."
  (save-selected-window
    (when (eq major-mode 'verifast-mode)
      (select-window (get-buffer-window next-error-last-buffer))
      (when (save-excursion
              (beginning-of-line)
              (looking-at " *-->"))
        (let ((start-of-error
               (save-excursion
                 (beginning-of-line)
                 (while (not (looking-at "^[a-z]+:\\|^[a-z]+\\[E[0-9]+\\]:"))
                   (forward-line -1))
                 (point))))
          (set-window-start (selected-window) start-of-error))))))

(eval-after-load 'compile
  '(progn
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'verifastc-new verifastc-new-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist 'verifastc-new)
     (add-hook 'next-error-hook 'verifastc-scroll-down-after-next-error)
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'verifastc verifastc-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist 'verifastc)
     (add-to-list 'compilation-error-regexp-alist-alist
                  (cons 'cargo cargo-compilation-regexps))
     (add-to-list 'compilation-error-regexp-alist 'cargo)))

;;; Functions to submit (parts of) buffers to the verifast playpen, for
;;; sharing.
(defun verifast-playpen-region (begin end)
  "Create a sharable URL for the contents of the current region
   on the Verifast playpen."
  (interactive "r")
  (let* ((data (buffer-substring begin end))
         (escaped-data (url-hexify-string data))
         (escaped-playpen-url (url-hexify-string (format verifast-playpen-url-format escaped-data))))
    (if (> (length escaped-playpen-url) 5000)
        (error "encoded playpen data exceeds 5000 character limit (length %s)"
               (length escaped-playpen-url))
      (let ((shortener-url (format verifast-shortener-url-format escaped-playpen-url))
            (url-request-method "POST"))
        (url-retrieve shortener-url
                      (lambda (state)
                        ; filter out the headers etc. included at the
                        ; start of the buffer: the relevant text
                        ; (shortened url or error message) is exactly
                        ; the last line.
                        (goto-char (point-max))
                        (let ((last-line (thing-at-point 'line t))
                              (err (plist-get state :error)))
                          (kill-buffer)
                          (if err
                              (error "failed to shorten playpen url: %s" last-line)
                            (message "%s" last-line)))))))))

(defun verifast-playpen-buffer ()
  "Create a sharable URL for the contents of the current buffer
   on the Verifast playpen."
  (interactive)
  (verifast-playpen-region (point-min) (point-max)))

(defun verifast-promote-module-into-dir ()
  "Promote the module file visited by the current buffer into its own directory.

For example, if the current buffer is visiting the file `foo.rs',
then this function creates the directory `foo' and renames the
file to `foo/mod.rs'.  The current buffer will be updated to
visit the new file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer is not visiting a file.")
      (if (string-equal (file-name-nondirectory filename) "mod.rs")
          (message "Won't promote a module file already named mod.rs.")
        (let* ((basename (file-name-sans-extension
                          (file-name-nondirectory filename)))
               (mod-dir (file-name-as-directory
                         (concat (file-name-directory filename) basename)))
               (new-name (concat mod-dir "mod.rs")))
          (mkdir mod-dir t)
          (rename-file filename new-name 1)
          (set-visited-file-name new-name))))))

(provide 'verifast-mode)

;;; verifast-mode.el ends here
