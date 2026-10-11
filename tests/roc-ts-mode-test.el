;;; roc-ts-mode-test.el --- Roc programming language mode tests -*- lexical-binding: t; -*-
;;; Code:

(require 'roc-ts-mode)

(defconst roc-ts-mode-test--dir (if load-file-name
                                    (file-name-directory load-file-name)
                                  default-directory))

(ert-deftest indent-examples ()
  "Check that roc-ts-mode indentation works correctly."
  (ert-test-erts-file (expand-file-name "./roc-ts-mode-examples.erts" roc-ts-mode-test--dir)
                      (lambda ()
                        (roc-ts-mode)
                        (when (and (bound-and-true-p dtrt-indent-mode) (fboundp 'dtrt-indent-mode))
                          (dtrt-indent-mode -1))
                        (indent-region (point-min) (point-max))
                        (funcall (if indent-tabs-mode #'tabify #'untabify) (point-min) (point-max)))))

(ert-deftest confirm-roc-ts-format ()
  "Check that the roc fmt command produces the same result.

This is not a test of roc-ts-mode itself; it's just testing that our
indentation examples in ./roc-ts-mode-examples.erts are still
up-to-date with the output of Roc's formatter."
  (ert-test-erts-file (expand-file-name "./roc-ts-mode-examples.erts" roc-ts-mode-test--dir)
                      (lambda ()
                        (shell-command-on-region (point-min) (point-max)
                                                 "roc fmt --stdin"
                                                 (current-buffer) t))))

(ert-deftest roc-ts-newline-and-indent ()
  (ert-test-erts-file (expand-file-name "./roc-ts-mode-newline-and-indent.erts" roc-ts-mode-test--dir)))

(defconst roc-ts-mode-test--syntax-file
  (expand-file-name "./all_syntax_test.roc" roc-ts-mode-test--dir)
  "Fixture demonstrating the new Roc compiler's syntax.
Copied from the Roc compiler repo: test/echo/all_syntax_test.roc.")

(defun roc-ts-mode-test--fixture-face-at (needle)
  "Fontify the syntax fixture and return the face at the start of NEEDLE.
NEEDLE is a literal string; the face is taken at the first character of
its first occurrence in the fixture."
  (with-temp-buffer
    (insert-file-contents roc-ts-mode-test--syntax-file)
    (roc-ts-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward needle)
    (get-text-property (match-beginning 0) 'face)))

(ert-deftest roc-ts-font-lock-function-declarations ()
  "Highlight lambda declarations and function annotations as functions."
  (with-temp-buffer
    (insert "main! : List(Str) => Try({}, _)\n"
            "main! = |_args| Ok({})\n"
            "parse : Str -> Try(Str, _)\n"
            "parse = |input| {\n"
            "    lines = input.trim().split_on(\"\\n\")\n"
            "    ok = |value| Ok(value)\n"
            "    ok(lines)\n"
            "}\n")
    (roc-ts-mode)
    (font-lock-ensure)
    (dolist (entry '(("main! :" . font-lock-function-name-face)
                     ("main! =" . font-lock-function-name-face)
                     ("parse :" . font-lock-function-name-face)
                     ("parse =" . font-lock-function-name-face)
                     ("lines =" . font-lock-variable-name-face)
                     ("ok =" . font-lock-function-name-face)))
      (goto-char (point-min))
      (search-forward (car entry))
      (should (eq (get-text-property (match-beginning 0) 'face)
                  (cdr entry))))))

(ert-deftest roc-ts-font-lock-else ()
  "`else' is highlighted as a keyword, same as `if'.
Regression test: the \"else\" rule was left commented out when the
font-lock rules were ported to the new grammar."
  (should (eq (roc-ts-mode-test--fixture-face-at "else {")
              'font-lock-keyword-face)))

(ert-deftest roc-ts-font-lock-keywords ()
  "New-syntax keywords all get `font-lock-keyword-face'."
  (dolist (needle '("if str == "      ; if_expr
                    "match color"     ; (match) named node
                    "for num in"      ; for_expr
                    "in num_list"     ; for_expr's `in'
                    "var $sum"        ; var_declaration
                    "return 99"       ; early_return_expr
                    "dbg foo"         ; dbg_expr
                    "expect Bool.True" ; top-level expect
                    "import \""       ; import statement
                    "as readme"       ; (as) named node in imports
                    "where [a.to_str")) ; (where) clause
    (ert-info ((format "needle: %s" needle))
      (should (eq (roc-ts-mode-test--fixture-face-at needle)
                  'font-lock-keyword-face)))))

(ert-deftest roc-ts-font-lock-conditional-keywords ()
  "Keywords behind grammar-support checks get `font-lock-keyword-face'.
`while', `break', and `crash' are only in newer tree-sitter-roc
grammars; skip this test when the installed grammar predates them."
  (skip-unless (roc-ts--ts-conditional-keyword-rules))
  (dolist (needle '("while $count"    ; while_expr
                    "break\n"         ; (break_expr); first bare `break' statement
                    "crash \"not"))   ; crash_expr
    (ert-info ((format "needle: %s" needle))
      (should (eq (roc-ts-mode-test--fixture-face-at needle)
                  'font-lock-keyword-face)))))

(ert-deftest roc-ts-font-lock-expect-statement ()
  "`expect' in statement position gets `font-lock-keyword-face'.
Grammars without a named `expect' node can parse statement-position
`expect' differently; skip this test on those grammars."
  (skip-unless (ignore-errors
                 (treesit-query-compile 'roc '((expect) @kw) t)))
  (should (eq (roc-ts-mode-test--fixture-face-at "expect sum ==")
              'font-lock-keyword-face)))

(ert-deftest roc-ts-block-statements-with-updated-grammar ()
  "Highlight and navigate block statements added by the updated grammar."
  (skip-unless (ignore-errors
                 (treesit-query-compile 'roc '((expect) @expect) t)))
  (with-temp-buffer
    (insert "check = |input| {\n"
            "    Ok(value) = input\n"
            "    expect value == 1\n"
            "    value\n"
            "}\n")
    (roc-ts-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "Ok(value)")
    (let ((node (treesit-node-at (match-beginning 0))))
      (should (equal (treesit-node-type node) "identifier"))
      (should (equal (treesit-node-type (treesit-node-parent node)) "tag"))
      (should (equal (treesit-node-type (treesit-node-parent
                                        (treesit-node-parent node)))
                     "tag_pattern")))
    (search-forward "expect")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-keyword-face))
    (should-not (treesit-query-capture (treesit-buffer-root-node)
                                      '((ERROR) @error)))))

(ert-deftest roc-ts-font-lock-error-uses-default-face ()
  "Use the default face for text the grammar parses as ERROR."
  (with-temp-buffer
    (insert "Ok(input_program)) = Program.parse(puzzle_input)\n")
    (roc-ts-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "input_program))")
    (let ((pos (1- (match-end 0))))
      (should (treesit-query-capture (treesit-buffer-root-node)
                                     '((ERROR) @error)))
      (should (eq (get-text-property pos 'face) 'default)))))

(ert-deftest roc-ts-font-lock-tag-pattern-binding ()
  "Highlight a name bound inside a top-level tag pattern."
  (with-temp-buffer
    (insert "Ok(input_program) = Program.parse(puzzle_input)\n\n"
            "part_one = Machine.default().run(input_program)\n")
    (roc-ts-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "input_program")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-variable-name-face))))

(ert-deftest roc-ts-font-lock-tag-union-payload-types ()
  "Tag constructors and their payload types have distinct faces."
  (with-temp-buffer
    (insert "Instruction := [\n"
            "    Halt,\n"
            "    Half(Register),\n"
            "    Jump(I32),\n"
            "    JumpIfEven(Register, I32),\n"
            "].{}\n")
    (roc-ts-mode)
    (font-lock-ensure)
    (dolist (tag '("Halt" "Half" "Jump(" "JumpIfEven"))
      (goto-char (point-min))
      (search-forward tag)
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'font-lock-builtin-face)))
    (dolist (type '("Register" "I32"))
      (goto-char (point-min))
      (while (search-forward type nil t)
        (should (eq (get-text-property (match-beginning 0) 'face)
                    'font-lock-type-face))))))

(ert-deftest roc-ts-font-lock-other-faces ()
  "Non-keyword constructs keep their own faces."
  (should (eq (roc-ts-mode-test--fixture-face-at "open sesame")
              'font-lock-string-face))
  (should (eq (roc-ts-mode-test--fixture-face-at "0x5")
              'font-lock-number-face))
  (should (eq (roc-ts-mode-test--fixture-face-at "# binary operators")
              'font-lock-comment-face)))

(ert-deftest roc-ts-font-lock-multiline-string ()
  "Each `\\\\'-prefixed line of a multiline string gets `font-lock-string-face'."
  (dolist (needle '("\\\\Line 1"
                    "Line 2"))
    (ert-info ((format "needle: %s" needle))
      (should (eq (roc-ts-mode-test--fixture-face-at needle)
                  'font-lock-string-face)))))

(ert-deftest roc-ts-font-lock-nominal-constructor ()
  "Nominal constructor names use the type face without coloring payloads."
  (with-temp-buffer
    (insert "ok = |registers| Ok(Machine.(registers))\n"
            "wrapped = Instruction.(Half(A))\n")
    (let ((treesit-font-lock-level 4))
      (roc-ts-mode))
    (setq-local case-fold-search nil)
    (font-lock-ensure)
    (should-not (treesit-query-capture (treesit-buffer-root-node)
                                      '((ERROR) @error)))
    (dolist (entry '(("Machine" . font-lock-type-face)
                     ("Instruction" . font-lock-type-face)
                     ("Ok" . font-lock-builtin-face)
                     ("Half" . font-lock-builtin-face)
                     ("A" . font-lock-builtin-face)
                     ("registers))" . font-lock-variable-use-face)))
      (goto-char (point-min))
      (search-forward (car entry))
      (let ((start (match-beginning 0))
            (length (if (equal (car entry) "registers))")
                        (length "registers")
                      (length (car entry)))))
        (dotimes (offset length)
          (should (eq (get-text-property (+ start offset) 'face)
                      (cdr entry))))))))

(ert-deftest roc-ts-font-lock-nominal-destructure ()
  "Nominal destructure names use the type face; tags keep the builtin face."
  (with-temp-buffer
    (insert "step = |Machine.({ pc, a, b }), Program.(instructions)| {\n"
            "    Ok(instructions)\n"
            "}\n")
    (let ((treesit-font-lock-level 4))
      (roc-ts-mode))
    (setq-local case-fold-search nil)
    (font-lock-ensure)
    (should-not (treesit-query-capture (treesit-buffer-root-node)
                                      '((ERROR) @error)))
    (dolist (entry '(("Machine" . font-lock-type-face)
                     ("Program" . font-lock-type-face)
                     ("Ok" . font-lock-builtin-face)
                     ("instructions)|" . font-lock-variable-use-face)))
      (goto-char (point-min))
      (search-forward (car entry))
      (let ((start (match-beginning 0))
            (length (if (equal (car entry) "instructions)|")
                        (length "instructions")
                      (length (car entry)))))
        (dotimes (offset length)
          (should (eq (get-text-property (+ start offset) 'face)
                      (cdr entry))))))))

(provide 'roc-ts-mode-test)
;;; roc-ts-mode-test.el ends here
