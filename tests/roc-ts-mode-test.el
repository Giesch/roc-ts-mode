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

(ert-deftest roc-ts-module-header-exposes-indentation ()
  "Indent a module header without changing import exposure indentation."
  (let ((roc-ts-indent-tabs-mode nil)
        (roc-ts-indent-offset 4))
    (dolist (case '(("module [\n    lower,\n    Upper,\n    ]\nvalue = 1\n"
                     "module [\n    lower,\n    Upper,\n]\nvalue = 1\n"
                     "module_exposes")
                    ("module [\n    lower,\n    Upper,\n]\n    value = 1\n"
                     "module [\n    lower,\n    Upper,\n]\nvalue = 1\n"
                     "module_exposes")
                    ("module [\n    lower,\n    Upper,\n]\nvalue = 1\n"
                     "module [\n    lower,\n    Upper,\n]\nvalue = 1\n"
                     "module_exposes")
                    ("module [main]\nimport Foo exposing [\n    lower,\n    Upper,\n]\nvalue = 1\n"
                     "module [main]\nimport Foo exposing [\n    lower,\n    Upper,\n]\nvalue = 1\n"
                     "exposes_list")))
      (with-temp-buffer
        (insert (car case))
        (roc-ts-mode)
        (goto-char (point-min))
        (search-forward "value = 1")
        (search-backward "]")
        (let* ((bracket (treesit-node-at (point)))
               (parent (treesit-node-parent bracket)))
          (should (equal (treesit-node-type bracket) "]"))
          (should (equal (treesit-node-type parent) (nth 2 case))))
        (indent-for-tab-command)
        (should (= (current-indentation) 0))
        (goto-char (point-min))
        (search-forward "value = 1")
        (beginning-of-line)
        (let ((fallback-called nil)
              (original-indent (symbol-function 'treesit-indent)))
          (cl-letf (((symbol-function 'treesit-indent)
                     (lambda (&rest args)
                       (setq fallback-called t)
                       (apply original-indent args))))
            (indent-for-tab-command))
          ;; After a module header, the normal mode rule must select the
          ;; module_exposes condition rather than reach generic indentation.
          (when (equal (nth 2 case) "module_exposes")
            (should-not fallback-called)))
        (should (equal (buffer-string) (nth 1 case)))))))

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

(ert-deftest roc-ts-defun-names ()
  "Read declaration names without depending on child positions."
  (dolist (case '(("f = |x| x" "value_declaration" "f")
                  ("(a, b) = t" "value_declaration" "(a, b)")
                  ("var n = 0" "var_declaration" "n")
                  ("T : [A]" "alias_type_def" "T")
                  ("Box(a) : List(a)" "alias_type_def" "Box")
                  ("N := [B]" "nominal_type_def" "N")
                  ("Secret(a) :: List(a)" "opaque_type_def" "Secret")
                  ("f : U64 -> U64" "annotation_type_def" nil)))
    (with-temp-buffer
      (insert (car case) "\n")
      (roc-ts-mode)
      (let* ((root (treesit-buffer-root-node))
             (node (car (treesit-filter-child
                         root (lambda (child)
                                (equal (treesit-node-type child) (nth 1 case)))))))
        (should-not (treesit-search-subtree root "ERROR"))
        (should node)
        (should (roc-ts--ts-defun-p node))
        (should (equal (roc-ts--ts-defun-name node) (nth 2 case)))))))

(ert-deftest roc-ts-imenu-declarations ()
  "Index declarations by category, without duplicate annotation entries."
  (with-temp-buffer
    (insert "f : U64 -> U64\nf = |x| x\nT : [A]\nN := [B]\n"
            "Secret :: [C]\nrun = || {\nvar n = 0\nn\n}\n")
    (roc-ts-mode)
    (should-not (treesit-search-subtree (treesit-buffer-root-node) "ERROR"))
    (let ((index (treesit-simple-imenu)))
      (dolist (entry '(("Definition" "f") ("Variable" "n")
                       ("Type alias" "T") ("Nominal type" "N")
                       ("Opaque type" "Secret")))
        (should (assoc (cadr entry) (cdr (assoc (car entry) index)))))
      (should (= 1 (cl-count "f" (cdr (assoc "Definition" index))
                             :key #'car :test #'equal))))
    (let ((variable (treesit-search-subtree (treesit-buffer-root-node)
                                            "var_declaration")))
      (should (equal (treesit-node-type (treesit-node-parent variable))
                     "body_expression"))
      (should (roc-ts--ts-defun-p variable)))))

(ert-deftest roc-ts-imenu-nominal-type ()
  "List nominal types by name in the imenu index."
  (with-temp-buffer
    (insert "Color := [Red, Green]\nx = 1\n")
    (roc-ts-mode)
    (let ((nominal-types (assoc "Nominal type" (treesit-simple-imenu))))
      (should nominal-types)
      (should (assoc "Color" (cdr nominal-types))))))

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

(ert-deftest roc-ts-font-lock-function-declaration-with-where-clause ()
  "Highlight a `where`-constrained function annotation the same as its definition."
  (with-temp-buffer
    (insert "advance : { n : c, sum : d, .. } -> [Ok((d, { n : c, sum : d }))]\n"
            "    where [c.plus : c, e -> c, d.plus : d, c -> d, e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)])]\n"
            "advance = |state| {\n"
            "    n = state.n + 1\n"
            "    sum = state.sum + state.n\n\n"
            "    Ok((sum, { sum, n }))\n"
            "}\n")
    (roc-ts-mode)
    (font-lock-ensure)
    (dolist (entry '(("advance :" . font-lock-function-name-face)
                     ("advance =" . font-lock-function-name-face)))
      (goto-char (point-min))
      (search-forward (car entry))
      (should (eq (get-text-property (match-beginning 0) 'face)
                  (cdr entry))))))

(ert-deftest roc-ts-font-lock-constraint-alias-references ()
  "Highlight constraint alias names as types without parser recovery."
  (with-temp-buffer
    (insert "item.Eq : where [item.is_eq : item, item -> Bool]\n"
            "item.Hash : where [item.to_hash : item, Hasher -> Hasher]\n"
            "powerset : List(item) -> Iter(Set(item))\n"
            "    where [item.Eq, item.Hash]\n")
    (roc-ts-mode)
    (should-not (treesit-search-subtree
                 (treesit-buffer-root-node 'roc) "ERROR"))
    (font-lock-ensure)
    (goto-char (point-min))
    (while (re-search-forward "item\\.\\(Eq\\|Hash\\)" nil t)
      (let ((start (match-beginning 1))
            (end (match-end 1)))
        (while (< start end)
          (should (eq (get-text-property start 'face)
                      'font-lock-type-face))
          (setq start (1+ start)))))))

(defun roc-ts-mode-test--assert-face (text face &optional start)
  "Assert FACE on every character of TEXT at or after START.
START defaults to the beginning of the buffer."
  (save-excursion
    (goto-char (or start (point-min)))
    (search-forward text)
    (let ((start (match-beginning 0)))
      (dotimes (offset (length text))
        (ert-info ((format "%S character %d" text offset))
          (should (eq (get-text-property (+ start offset) 'face) face)))))))

(ert-deftest roc-ts-font-lock-package-namespaces ()
  "Style dependency aliases as namespaces without coloring their payloads."
  (dolist (header '("app [main] { pf: platform \"gui.tar.zst\", util: \"util.tar.zst\", }\n"
                    "package [Main] { util: \"util.tar.zst\", }\n"))
    (with-temp-buffer
      (insert header
              "import util.Helpers\n"
              "import Local as Alias\n"
              "main = util\n")
      (let ((treesit-font-lock-level 4))
        (roc-ts-mode))
      (font-lock-ensure)
      (should-not (treesit-search-subtree (treesit-buffer-root-node 'roc) "ERROR"))
      (when (string-prefix-p "app" header)
        (roc-ts-mode-test--assert-face "pf" 'font-lock-constant-face)
        (roc-ts-mode-test--assert-face "platform" 'font-lock-keyword-face)
        (roc-ts-mode-test--assert-face "\"gui.tar.zst\"" 'font-lock-string-face))
      (roc-ts-mode-test--assert-face "util" 'font-lock-constant-face)
      (roc-ts-mode-test--assert-face "\"util.tar.zst\"" 'font-lock-string-face)
      (roc-ts-mode-test--assert-face ":" 'font-lock-delimiter-face)
      (roc-ts-mode-test--assert-face "," 'font-lock-delimiter-face)
      (roc-ts-mode-test--assert-face "{" 'font-lock-bracket-face)
      (roc-ts-mode-test--assert-face "}" 'font-lock-bracket-face)
      (goto-char (point-min))
      (search-forward "import ")
      (roc-ts-mode-test--assert-face "util" 'font-lock-constant-face (point))
      (roc-ts-mode-test--assert-face "." 'font-lock-operator-face (point))
      (roc-ts-mode-test--assert-face "Helpers" 'font-lock-constant-face)
      (roc-ts-mode-test--assert-face "Local" 'font-lock-constant-face)
      (roc-ts-mode-test--assert-face "Alias" 'font-lock-constant-face)
      (search-forward "main = ")
      (roc-ts-mode-test--assert-face "util" 'font-lock-variable-use-face (point))))
  (with-temp-buffer
    (insert "app [main] { pf: platform \"gui.tar.zst\" }\nimport pf.Gui\nmain = 0\n")
    (let ((treesit-font-lock-level 4))
      (roc-ts-mode))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "pf:")
    (replace-match "gui:" t t)
    (font-lock-ensure)
    (roc-ts-mode-test--assert-face "gui" 'font-lock-constant-face)))

(ert-deftest roc-ts-font-lock-exposed-values-functions-and-types ()
  "Style exposed types without guessing whether lowercase names are functions.
Even a locally defined function uses the exposed-variable face in the header;
its definition and calls retain their independently determined function faces."
  (dolist (source '("module [main]\nimport pf.Gui exposing [value, render!, Action]\nmain = render!(value)\n"
                    "module [value, render!, Action]\nvalue = 42\nrender! = |state| state\nAction := [None].{}\nmain = render!(value)\n"))
    (with-temp-buffer
      (insert source)
      (let ((treesit-font-lock-level 4))
        (roc-ts-mode))
      (font-lock-ensure)
      (should-not (treesit-search-subtree (treesit-buffer-root-node 'roc) "ERROR"))
      (dolist (entry '(("value" . font-lock-variable-use-face)
                       ("render!" . font-lock-variable-use-face)
                       ("Action" . font-lock-type-face)))
        (roc-ts-mode-test--assert-face (car entry) (cdr entry)))
      (goto-char (point-min))
      (search-forward "main = render!")
      (should (eq (get-text-property (1- (point)) 'face)
                  'font-lock-function-call-face))
      (when (string-match-p "render! =" source)
        (goto-char (point-min))
        (search-forward "render! =")
        (should (eq (get-text-property (- (point) (length "render! =")) 'face)
                    'font-lock-function-name-face))))))

(ert-deftest roc-ts-font-lock-rich-exposed-names ()
  "Keep exposed-name faces on leaves, preserving namespaces and punctuation."
  (dolist (header '("module [%s]\nmain = 0\n"
                    "module [main]\nimport pf.Gui as G exposing [%s]\nmain = 0\n"))
    (dolist (case '(("value as renamed"
                     ("value" . font-lock-variable-use-face)
                     ("renamed" . font-lock-variable-use-face)
                     ("as" . font-lock-keyword-face))
                    ("render! as draw!"
                     ("render!" . font-lock-variable-use-face)
                     ("draw!" . font-lock-variable-use-face))
                    ("Action as Message"
                     ("Action" . font-lock-type-face)
                     ("Message" . font-lock-type-face))
                    ("Outer.Inner.Action"
                     ("Outer" . font-lock-constant-face)
                     ("Inner" . font-lock-constant-face)
                     ("Action" . font-lock-type-face))
                    ("Outer.value.Deep.render!"
                     ("Outer" . font-lock-constant-face)
                     ("value" . font-lock-variable-use-face)
                     ("Deep" . font-lock-constant-face)
                     ("render!" . font-lock-variable-use-face))
                    ("Outer.Action as Message"
                     ("Outer" . font-lock-constant-face)
                     ("Action" . font-lock-type-face)
                     ("Message" . font-lock-type-face))
                    ("Outer.render! as Alias"
                     ("Outer" . font-lock-constant-face)
                     ("render!" . font-lock-variable-use-face)
                     ("Alias" . font-lock-type-face))
                    ("Action.*"
                     ("Action" . font-lock-type-face)
                     (".*" . font-lock-operator-face))
                    ("Outer.Action.*"
                     ("Outer" . font-lock-constant-face)
                     ("Action" . font-lock-type-face)
                     (".*" . font-lock-operator-face))
                    ("Outer.value.*"
                     ("Outer" . font-lock-constant-face)
                     ("value" . font-lock-variable-use-face)
                     (".*" . font-lock-operator-face))))
      (ert-info ((format "%s: %s" header (car case)))
        (with-temp-buffer
          (insert (format header (car case)))
          (let ((treesit-font-lock-level 4))
            (roc-ts-mode))
          (font-lock-ensure)
          (should-not (treesit-search-subtree (treesit-buffer-root-node 'roc) "ERROR"))
          (goto-char (point-min))
          (search-forward (car case))
          (let ((item-start (match-beginning 0)))
            (dolist (entry (cdr case))
              (roc-ts-mode-test--assert-face (car entry) (cdr entry) item-start)))
          (when (string-match-p "import" header)
            (roc-ts-mode-test--assert-face "Gui" 'font-lock-constant-face)
            (goto-char (point-min))
            (search-forward "as G")
            (should (eq (get-text-property (1- (point)) 'face)
                        'font-lock-constant-face)))
          (goto-char (point-min))
          (while (search-forward "." nil t)
            (should (eq (get-text-property (1- (point)) 'face)
                        'font-lock-operator-face))))))))

(ert-deftest roc-ts-font-lock-exposed-types-levels-and-edits ()
  "Apply the type feature at levels 2–4 and discard stale faces after edits."
  (dolist (level '(2 3 4))
    (with-temp-buffer
      (insert "module [main]\nimport pf.Gui exposing [\n    Action, # Action comment\n]\nmain = \"Action string\"\n")
      (let ((treesit-font-lock-level level))
        (roc-ts-mode))
      (font-lock-ensure)
      (should-not (treesit-search-subtree (treesit-buffer-root-node 'roc) "ERROR"))
      (roc-ts-mode-test--assert-face "Action" 'font-lock-type-face)
      (roc-ts-mode-test--assert-face "Action comment" 'font-lock-comment-face)
      (roc-ts-mode-test--assert-face "Action string" 'font-lock-string-face)
      (goto-char (point-min))
      (search-forward "Action")
      (replace-match "action" t t)
      (font-lock-ensure)
      (roc-ts-mode-test--assert-face "action" (and (= level 4) 'font-lock-variable-use-face))
      (goto-char (point-min))
      (search-forward "action")
      (replace-match "Action" t t)
      (font-lock-ensure)
      (roc-ts-mode-test--assert-face "Action" 'font-lock-type-face))))

(ert-deftest roc-ts-font-lock-effectful-function-calls ()
  "Use normal function faces for effectful declarations and calls."
  (with-temp-buffer
    (insert "main! : List(Str) => Try({}, _)\n"
            "main! = |_args| {\n"
            "    part_one = solve({ difficulty: Normal })\n"
            "    echo!(\"Part One: ${part_one.to_str()}\\n\")\n"
            "    Ok({})\n"
            "}\n")
    (let ((treesit-font-lock-level 4))
      (roc-ts-mode)
      (font-lock-ensure)
    (dolist (entry '(("main! :" . font-lock-function-name-face)
                     ("main! =" . font-lock-function-name-face)
                     ("echo!(" . font-lock-function-call-face)
                     ("solve(" . font-lock-function-call-face)))
      (goto-char (point-min))
      (search-forward (car entry))
      (let ((start (match-beginning 0)))
        (should (eq (get-text-property start 'face) (cdr entry)))
        (when (string-match-p "!" (car entry))
          (should (eq (get-text-property
                       (+ start (string-match-p "!" (car entry))) 'face)
                      (cdr entry)))))))))

(ert-deftest roc-ts-font-lock-method-calls ()
  "Highlight called fields as functions and field reads as property uses."
  (with-temp-buffer
    (insert "parse = |input| {\n"
            "    parts = input.trim().split_on(\"\\n\")\n"
            "    name = input.name\n"
            "    parts.get(0)\n"
            "}\n")
    (let ((treesit-font-lock-level 4))
      (roc-ts-mode)
      (font-lock-ensure)
      (dolist (entry '(("trim(" . font-lock-function-call-face)
                       ("split_on(" . font-lock-function-call-face)
                       ("get(" . font-lock-function-call-face)
                       ("name\n" . font-lock-property-use-face)))
        (goto-char (point-min))
        (search-forward (car entry))
        (should (eq (get-text-property (match-beginning 0) 'face)
                    (cdr entry)))))))

(ert-deftest roc-ts-font-lock-mutable-declaration ()
  "Highlight the name introduced by `var' as a declaration."
  (with-temp-buffer
    (insert "solve = |_| {\n"
            "    var $battles = []\n"
            "    $battles\n"
            "}\n")
    (roc-ts-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "$battles")
    (let ((start (match-beginning 0)))
      (should (equal (treesit-node-type (treesit-node-at (1+ start)))
                     "identifier"))
      (should (eq (get-text-property start 'face)
                  'font-lock-variable-name-face))
      (should (eq (get-text-property (1+ start) 'face)
                  'font-lock-variable-name-face)))))

(ert-deftest roc-ts-font-lock-tuple-declaration ()
  "Highlight both names introduced by a tuple declaration."
  (with-temp-buffer
    (insert "solve = |_| {\n"
            "    old_buffs = battle.player.buff_clocks\n"
            "    (armor, shield) = (7, old_buffs.shield)\n"
            "    armor + shield\n"
            "}\n")
    (roc-ts-mode)
    (font-lock-ensure)
    (dolist (name '("old_buffs" "armor" "shield"))
      (goto-char (point-min))
      (search-forward name)
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'font-lock-variable-name-face)))))

(ert-deftest roc-ts-font-lock-nominal-declaration ()
  "Highlight a name bound by a nominal destructuring declaration."
  (with-temp-buffer
    (insert "tick_buffs = |battle| {\n"
            "    Player.(old_player) = battle.player\n"
            "    Player.(old_player)\n"
            "}\n")
    (let ((treesit-font-lock-level 4))
      (roc-ts-mode))
    (font-lock-ensure)
    (should-not (treesit-query-capture (treesit-buffer-root-node)
                                      '((ERROR) @error)))
    (goto-char (point-min))
    (search-forward "Player.(old_player) =")
    (let ((start (match-beginning 0)))
      (should (eq (get-text-property start 'face) 'font-lock-type-face))
      (should (eq (get-text-property (+ start (length "Player.(")) 'face)
                  'font-lock-variable-name-face)))
    (search-forward "old_player")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-variable-use-face))))

(ert-deftest roc-ts-font-lock-else ()
  "`else' is highlighted as a keyword, same as `if'.
Regression test: the \"else\" rule was left commented out when the
font-lock rules were ported to the new grammar."
  (should (eq (roc-ts-mode-test--fixture-face-at "else {")
              'font-lock-keyword-face)))

(ert-deftest roc-ts-font-lock-keywords ()
  "Highlight keywords with `font-lock-keyword-face'."
  (dolist (needle '("if str == "      ; if_expr
                    "match color"     ; (match) named node
                    "for num in"      ; for_expr
                    "in num_list"     ; for_expr's `in'
                    "while $count"    ; while_expr
                    "break\n"        ; (break_expr); first bare `break' statement
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

(ert-deftest roc-ts-font-lock-crash ()
  "Highlight `crash' in bold red without changing its string argument."
  (should (eq (roc-ts-mode-test--fixture-face-at "crash \"not")
              'roc-ts-crash-face))
  (should (eq (roc-ts-mode-test--fixture-face-at "\"not implemented\"")
              'font-lock-string-face))
  (should (equal (face-attribute 'roc-ts-crash-face :foreground nil t)
                 (face-attribute 'font-lock-warning-face :foreground nil t)))
  (should (eq (face-attribute 'roc-ts-crash-face :weight nil t) 'bold)))

(ert-deftest roc-ts-font-lock-expect-statement ()
  "Highlight `expect' in statement position as a keyword."
  (should (eq (roc-ts-mode-test--fixture-face-at "expect sum ==")
              'font-lock-keyword-face)))

(ert-deftest roc-ts-block-statements ()
  "Highlight and navigate block statements."
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

(ert-deftest roc-ts-font-lock-early-return ()
  "Bold only the question mark in plain and error-wrapping early returns."
  (dolist (level '(3 4))
    (with-temp-buffer
      (insert "run = |input| {\n"
              "    plain = input.check()?\n"
              "    chained = input.check()?.tick().check()?\n"
              "    tagged = I32.from_str(input) ? InvalidOffset\n"
              "    wrapped = input.get(0) ?\n"
              "        |e| MissingLine(e)\n"
              "    ignored = input.get(1) ?\n"
              "        |_| MissingLine(1)\n"
              "    Ok(plain)\n"
              "}\n")
      (let ((treesit-font-lock-level level))
        (roc-ts-mode))
      (font-lock-ensure)
      (should-not (treesit-query-capture (treesit-buffer-root-node)
                                        '((ERROR) @error)))
      (goto-char (point-min))
      (let ((count 0))
        (while (search-forward "?" nil t)
          (setq count (1+ count))
          (should (eq (get-text-property (1- (point)) 'face)
                      'roc-ts-question-mark-face))
          (should-not (eq (get-text-property (- (point) 2) 'face)
                          'roc-ts-question-mark-face))
          (should-not (eq (get-text-property (point) 'face)
                          'roc-ts-question-mark-face)))
        (should (= count 6)))))
  (should (eq (face-attribute 'roc-ts-question-mark-face :weight nil t) 'bold))
  (should (equal (face-attribute 'roc-ts-question-mark-face :foreground nil t)
                 (face-attribute 'font-lock-keyword-face :foreground nil t))))

(ert-deftest roc-ts-font-lock-question-mark-exclusions ()
  "Default operators, strings, and comments keep their normal faces."
  (with-temp-buffer
    (insert "run = |input| {\n"
            "    value = input ?? 0\n"
            "    text = \"? ??\"\n"
            "    multiline =\n"
            "        \\\\literal ? ??\n"
            "    # comment ? ??\n"
            "    value\n"
            "}\n")
    (let ((treesit-font-lock-level 4))
      (roc-ts-mode))
    (font-lock-ensure)
    (should-not (treesit-query-capture (treesit-buffer-root-node)
                                      '((ERROR) @error)))
    (dolist (entry '(("input ??" . font-lock-operator-face)
                     ("\"? ??" . font-lock-string-face)
                     ("literal ? ??" . font-lock-string-face)
                     ("comment ? ??" . font-lock-comment-face)))
      (goto-char (point-min))
      (search-forward (car entry))
      (let ((end (point)))
        (goto-char (match-beginning 0))
        (while (search-forward "?" end t)
          (should (eq (get-text-property (1- (point)) 'face)
                      (cdr entry))))))))

(provide 'roc-ts-mode-test)
;;; roc-ts-mode-test.el ends here

(ert-deftest roc-ts-font-lock-derived-associated-methods ()
  "Only supported associated annotations without matching bodies are derives."
  (dolist (name '("is_eq" "to_hash" "parser_for" "encoder_for" "map" "map!"))
    (dolist (tail '("" "\n    other = 42" "\n    # comment\n    other = 42"))
      (with-temp-buffer
        (insert "Flags := [A, B].{\n    " name " : _" tail "\n}\n")
        (roc-ts-mode)
        (font-lock-ensure)
        (goto-char (point-min))
        (search-forward (concat name " :"))
        (dotimes (offset (length name))
          (should (eq (get-text-property (+ (match-beginning 0) offset) 'face)
                      'font-lock-function-name-face)))
        (should-not (eq (get-text-property (+ (match-beginning 0) (length name)) 'face)
                        'font-lock-function-name-face))
        (search-forward "_")
        (should-not (eq (get-text-property (1- (point)) 'face)
                        'font-lock-function-name-face))))))

(ert-deftest roc-ts-font-lock-inferred-values-are-not-derives ()
  "An explicit definition or a non-associated context prevents derivation."
  (dolist (source '("Flags := [A].{ is_eq : _\n is_eq = 42 }"
                    "Flags := [A].{ is_eq : _\n # comment\n ## docs\n is_eq = 42 }"
                    "Flags := [A].{ count : _\n count = 42 }"
                    "Flags := [A].{ unknown : _ }"
                    "is_eq : _\nis_eq = 42"
                    "main = || { is_eq : _\n is_eq = 42\n is_eq }"))
    (with-temp-buffer
      (insert source)
      (roc-ts-mode)
      (font-lock-ensure)
      (goto-char (point-min))
      (re-search-forward "[a-z_]+ : _")
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'font-lock-variable-name-face)))))

(ert-deftest roc-ts-font-lock-derived-method-edit-definition ()
  "Adding and removing an explicit body updates the preceding annotation."
  (with-temp-buffer
    (insert "Flags := [A].{\n    is_eq : _\n}\n")
    (roc-ts-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "is_eq")
    (let ((name-start (match-beginning 0)))
      (should (eq (get-text-property name-start 'face) 'font-lock-function-name-face))
      (end-of-line)
      (let ((body-start (point)))
        (insert "\n    is_eq = 42")
        (font-lock-ensure)
        (should (eq (get-text-property name-start 'face) 'font-lock-variable-name-face))
        (delete-region body-start (point))
        (font-lock-ensure)
        (should (eq (get-text-property name-start 'face) 'font-lock-function-name-face))))))
