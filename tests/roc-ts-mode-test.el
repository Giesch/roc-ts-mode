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
  "Check that the roc format command produces the same result.

This is not a test of roc-ts-mode itself; it's just testing that our
indentation examples in ./roc-ts-mode-examples.erts are still
up-to-date with the output of Roc's formatter."
  (ert-test-erts-file (expand-file-name "./roc-ts-mode-examples.erts" roc-ts-mode-test--dir)
                      (lambda ()
                        (shell-command-on-region (point-min) (point-max)
                                                 "roc fmt --stdin --stdout"
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

(ert-deftest roc-ts-font-lock-other-faces ()
  "Non-keyword constructs keep their own faces."
  (should (eq (roc-ts-mode-test--fixture-face-at "open sesame")
              'font-lock-string-face))
  (should (eq (roc-ts-mode-test--fixture-face-at "0x5")
              'font-lock-number-face))
  (should (eq (roc-ts-mode-test--fixture-face-at "# binary operators")
              'font-lock-comment-face)))

(provide 'roc-ts-mode-test)
;;; roc-ts-mode-test.el ends here
