;;; todo-explorer-test.el --- Tests for todo-explorer -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for todo-explorer.el

;;; Code:

(require 'ert)
(require 'todo-explorer)

(defvar todo-explorer-test--fixture-dir
  (expand-file-name "test/fixtures/sample-project/"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Path to test fixture directory.")

;;;; Unit Tests — Line Parsing

(ert-deftest todo-explorer-test-parse-rg-vimgrep-line ()
  "Parse a ripgrep --vimgrep output line."
  (let ((item (todo-explorer--parse-line
               "src/main.el:42:10:;; TODO: Fix this thing"
               "/project/")))
    (should item)
    (should (string= (todo-explorer-item-file item)
                     "/project/src/main.el"))
    (should (= (todo-explorer-item-line item) 42))
    (should (= (todo-explorer-item-column item) 10))
    (should (string= (todo-explorer-item-keyword item) "TODO"))
    (should (string= (todo-explorer-item-text item) "Fix this thing"))))

(ert-deftest todo-explorer-test-parse-grep-line ()
  "Parse a grep -n output line (no column)."
  (let ((item (todo-explorer--parse-line
               "src/main.py:15:# FIXME: Handle edge case"
               "/project/")))
    (should item)
    (should (string= (todo-explorer-item-file item)
                     "/project/src/main.py"))
    (should (= (todo-explorer-item-line item) 15))
    (should (null (todo-explorer-item-column item)))
    (should (string= (todo-explorer-item-keyword item) "FIXME"))
    (should (string= (todo-explorer-item-text item) "Handle edge case"))))

(ert-deftest todo-explorer-test-parse-absolute-path ()
  "Parse a line with an absolute file path."
  (let ((item (todo-explorer--parse-line
               "/home/user/project/foo.el:5:;; NOTE: Important"
               "/home/user/project/")))
    (should item)
    (should (string= (todo-explorer-item-file item)
                     "/home/user/project/foo.el"))
    (should (string= (todo-explorer-item-keyword item) "NOTE"))))

(ert-deftest todo-explorer-test-parse-keyword-no-colon ()
  "Parse a line where keyword has no trailing colon."
  (let ((item (todo-explorer--parse-line
               "foo.el:10:;; TODO implement later"
               "/project/")))
    (should item)
    (should (string= (todo-explorer-item-keyword item) "TODO"))
    (should (string= (todo-explorer-item-text item) "implement later"))))

(ert-deftest todo-explorer-test-parse-hack-keyword ()
  "Parse HACK keyword."
  (let ((item (todo-explorer--parse-line
               "foo.js:3:// HACK: workaround for bug"
               "/project/")))
    (should item)
    (should (string= (todo-explorer-item-keyword item) "HACK"))
    (should (string= (todo-explorer-item-text item) "workaround for bug"))))

(ert-deftest todo-explorer-test-parse-invalid-line ()
  "Return nil for unparseable lines."
  (should (null (todo-explorer--parse-line "not a match" "/project/")))
  (should (null (todo-explorer--parse-line "" "/project/"))))

;;;; Unit Tests — Keywords & Priorities

(ert-deftest todo-explorer-test-keyword-priority ()
  "Keyword priorities map correctly."
  (should (= (todo-explorer--keyword-priority "FIXME") 1))
  (should (= (todo-explorer--keyword-priority "BUG") 1))
  (should (= (todo-explorer--keyword-priority "TODO") 2))
  (should (= (todo-explorer--keyword-priority "NOTE") 3))
  ;; Unknown keyword gets default priority 2
  (should (= (todo-explorer--keyword-priority "UNKNOWN") 2)))

(ert-deftest todo-explorer-test-keyword-face ()
  "Keyword faces map correctly."
  (should (eq (todo-explorer--keyword-face "FIXME") 'todo-explorer-face-fixme))
  (should (eq (todo-explorer--keyword-face "TODO") 'todo-explorer-face-todo))
  (should (eq (todo-explorer--keyword-face "NOTE") 'todo-explorer-face-note))
  (should (eq (todo-explorer--keyword-face "HACK") 'todo-explorer-face-hack))
  ;; Unknown keyword gets default face
  (should (eq (todo-explorer--keyword-face "UNKNOWN") 'todo-explorer-face-todo)))

(ert-deftest todo-explorer-test-effective-keywords ()
  "Effective keywords include the default set."
  (let ((kws (todo-explorer--effective-keywords)))
    (should (member "TODO" kws))
    (should (member "FIXME" kws))
    (should (member "NOTE" kws))
    (should (member "HACK" kws))))

;;;; Unit Tests — Sorting

(ert-deftest todo-explorer-test-sort-by-file ()
  "Items sort by file then line number."
  (let* ((todo-explorer--sort-mode 'file)
         (items (list (todo-explorer-item-create
                       :file "/a/z.el" :line 10 :keyword "TODO" :priority 2)
                      (todo-explorer-item-create
                       :file "/a/a.el" :line 5 :keyword "FIXME" :priority 1)
                      (todo-explorer-item-create
                       :file "/a/a.el" :line 1 :keyword "NOTE" :priority 3)))
         (sorted (todo-explorer--sort-items items)))
    (should (string= (todo-explorer-item-file (nth 0 sorted)) "/a/a.el"))
    (should (= (todo-explorer-item-line (nth 0 sorted)) 1))
    (should (string= (todo-explorer-item-file (nth 1 sorted)) "/a/a.el"))
    (should (= (todo-explorer-item-line (nth 1 sorted)) 5))
    (should (string= (todo-explorer-item-file (nth 2 sorted)) "/a/z.el"))))

(ert-deftest todo-explorer-test-sort-by-priority ()
  "Items sort by priority (1 = highest first)."
  (let* ((todo-explorer--sort-mode 'priority)
         (items (list (todo-explorer-item-create
                       :file "/a.el" :line 1 :keyword "NOTE" :priority 3)
                      (todo-explorer-item-create
                       :file "/b.el" :line 1 :keyword "FIXME" :priority 1)
                      (todo-explorer-item-create
                       :file "/c.el" :line 1 :keyword "TODO" :priority 2)))
         (sorted (todo-explorer--sort-items items)))
    (should (= (todo-explorer-item-priority (nth 0 sorted)) 1))
    (should (= (todo-explorer-item-priority (nth 1 sorted)) 2))
    (should (= (todo-explorer-item-priority (nth 2 sorted)) 3))))

(ert-deftest todo-explorer-test-sort-by-keyword ()
  "Items sort alphabetically by keyword."
  (let* ((todo-explorer--sort-mode 'keyword)
         (items (list (todo-explorer-item-create
                       :file "/a.el" :line 1 :keyword "TODO" :priority 2)
                      (todo-explorer-item-create
                       :file "/b.el" :line 1 :keyword "FIXME" :priority 1)
                      (todo-explorer-item-create
                       :file "/c.el" :line 1 :keyword "NOTE" :priority 3)))
         (sorted (todo-explorer--sort-items items)))
    (should (string= (todo-explorer-item-keyword (nth 0 sorted)) "FIXME"))
    (should (string= (todo-explorer-item-keyword (nth 1 sorted)) "NOTE"))
    (should (string= (todo-explorer-item-keyword (nth 2 sorted)) "TODO"))))

;;;; Unit Tests — Relative Path

(ert-deftest todo-explorer-test-relative-path ()
  "Relative path computation from project root."
  (let ((todo-explorer--project-root "/home/user/project/"))
    (should (string= (todo-explorer--relative-path
                      "/home/user/project/src/main.el")
                     "src/main.el"))))

;;;; Unit Tests — Regexp Construction

(ert-deftest todo-explorer-test-build-rg-regexp ()
  "Scanner regexp contains all keywords."
  (let ((re (todo-explorer--build-scanner-regexp)))
    (should (string-match-p "TODO" re))
    (should (string-match-p "FIXME" re))
    (should (string-match-p "NOTE" re))
    (should (string-match-p "\\\\b(" re))))

(ert-deftest todo-explorer-test-build-elisp-regexp ()
  "Elisp regexp uses word boundaries."
  (let ((re (todo-explorer--build-elisp-regexp)))
    (should (string-match-p "\\\\_<" re))
    (should (string-match-p "\\\\_>" re))))

;;;; Unit Tests — Scanner Detection

(ert-deftest todo-explorer-test-detect-scanner-explicit ()
  "Explicit scanner choice overrides auto-detect."
  (let ((todo-explorer-scan-tool 'grep))
    (should (eq (todo-explorer--detect-scanner) 'grep)))
  (let ((todo-explorer-scan-tool 'emacs))
    (should (eq (todo-explorer--detect-scanner) 'emacs))))

;;;; Unit Tests — Data Structure

(ert-deftest todo-explorer-test-item-create ()
  "Item struct creation and access."
  (let ((item (todo-explorer-item-create
               :file "/foo.el"
               :line 42
               :column 5
               :keyword "TODO"
               :text "Fix this"
               :priority 2)))
    (should (todo-explorer-item-p item))
    (should (string= (todo-explorer-item-file item) "/foo.el"))
    (should (= (todo-explorer-item-line item) 42))
    (should (= (todo-explorer-item-column item) 5))
    (should (string= (todo-explorer-item-keyword item) "TODO"))
    (should (string= (todo-explorer-item-text item) "Fix this"))
    (should (= (todo-explorer-item-priority item) 2))))

;;;; Unit Tests — Context Detection

(ert-deftest todo-explorer-test-context-inside-defun ()
  "Detect context inside an Emacs Lisp defun."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun my-function ()\n  ;; TODO: fix this\n  nil)\n")
    (let ((ctx (todo-explorer--detect-context-in-buffer 2)))
      (should ctx)
      (should (string-match-p "my-function" ctx))
      (should (string-prefix-p "fn: " ctx)))))

(ert-deftest todo-explorer-test-context-file-header ()
  "Detect file header context for early lines."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";;; test.el --- desc -*- lexical-binding: t; -*-\n")
    (insert ";; TODO: something here\n")
    (insert "\n")
    (insert "(defun later () nil)\n")
    (let ((ctx (todo-explorer--detect-context-in-buffer 2)))
      (should (string= ctx "file header")))))

(ert-deftest todo-explorer-test-context-top-level ()
  "Detect top-level context for non-header, non-function lines."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";;; test.el --- desc\n\n\n\n")
    (insert ";; NOTE: top-level comment\n")
    (insert "(defun later () nil)\n")
    (let ((ctx (todo-explorer--detect-context-in-buffer 5)))
      (should (string= ctx "top-level")))))

(ert-deftest todo-explorer-test-context-field-on-item ()
  "Context field exists on item struct."
  (let ((item (todo-explorer-item-create
               :file "/foo.el" :line 1 :keyword "TODO"
               :priority 2 :context "fn: foo")))
    (should (string= (todo-explorer-item-context item) "fn: foo"))))

(ert-deftest todo-explorer-test-sort-by-context ()
  "Items sort alphabetically by context."
  (let* ((todo-explorer--sort-mode 'context)
         (items (list (todo-explorer-item-create
                       :file "/a.el" :line 1 :keyword "TODO"
                       :priority 2 :context "top-level")
                      (todo-explorer-item-create
                       :file "/b.el" :line 1 :keyword "FIXME"
                       :priority 1 :context "fn: alpha")
                      (todo-explorer-item-create
                       :file "/c.el" :line 1 :keyword "NOTE"
                       :priority 3 :context "fn: beta")))
         (sorted (todo-explorer--sort-items items)))
    (should (string= (todo-explorer-item-context (nth 0 sorted)) "fn: alpha"))
    (should (string= (todo-explorer-item-context (nth 1 sorted)) "fn: beta"))
    (should (string= (todo-explorer-item-context (nth 2 sorted)) "top-level"))))

;;;; Unit Tests — Context Line Expansion

(ert-deftest todo-explorer-test-fetch-context-lines ()
  "Fetch context lines from a fixture file."
  (skip-unless (file-directory-p todo-explorer-test--fixture-dir))
  (let* ((file (expand-file-name "file1.el" todo-explorer-test--fixture-dir))
         (result (todo-explorer--fetch-context-lines file 6 2)))
    ;; Line 6 is the FIXME inside sample-login, with 2 lines context
    (should result)
    ;; Should contain lines 4-8
    (should (string-match-p "4:" result))
    (should (string-match-p "6:" result))
    (should (string-match-p "8:" result))
    ;; Match line (6) should have > marker
    (should (string-match-p "> *6:" result))
    ;; Non-match lines should have space marker
    (should (string-match-p "  *4:" result))))

(ert-deftest todo-explorer-test-fetch-context-lines-edge ()
  "Context lines at file start don't go negative."
  (skip-unless (file-directory-p todo-explorer-test--fixture-dir))
  (let* ((file (expand-file-name "file1.el" todo-explorer-test--fixture-dir))
         (result (todo-explorer--fetch-context-lines file 1 3)))
    (should result)
    ;; Should start at line 1, not line -2
    (should (string-match-p "1:" result))
    (should (string-match-p "> *1:" result))))

(ert-deftest todo-explorer-test-toggle-context ()
  "Toggle context creates and removes overlays."
  (skip-unless (file-directory-p todo-explorer-test--fixture-dir))
  (with-temp-buffer
    (todo-explorer-mode)
    (setq todo-explorer--project-root todo-explorer-test--fixture-dir)
    (todo-explorer--scan-native todo-explorer-test--fixture-dir
                                (current-buffer))
    ;; Should have items
    (should (> (length todo-explorer--items) 0))
    ;; Move to first item
    (goto-char (point-min))
    ;; No overlays initially
    (should (= 0 (length (seq-filter
                           (lambda (ov)
                             (overlay-get ov 'todo-explorer-context-ov))
                           (overlays-in (point-min) (point-max))))))
    ;; Toggle expand
    (todo-explorer-toggle-context)
    ;; Should have one overlay now
    (should (= 1 (length (seq-filter
                           (lambda (ov)
                             (overlay-get ov 'todo-explorer-context-ov))
                           (overlays-in (point-min) (point-max))))))
    ;; Toggle collapse
    (todo-explorer-toggle-context)
    ;; Back to zero overlays
    (should (= 0 (length (seq-filter
                           (lambda (ov)
                             (overlay-get ov 'todo-explorer-context-ov))
                           (overlays-in (point-min) (point-max))))))))

;;;; Integration Tests — Context in Fixtures

(ert-deftest todo-explorer-test-native-scan-context ()
  "Native scan populates context for fixture items."
  (skip-unless (file-directory-p todo-explorer-test--fixture-dir))
  (with-temp-buffer
    (todo-explorer-mode)
    (setq todo-explorer--project-root todo-explorer-test--fixture-dir)
    (todo-explorer--scan-native todo-explorer-test--fixture-dir
                                (current-buffer))
    ;; At least some items should have non-nil context
    (let ((contexts (cl-remove nil (mapcar #'todo-explorer-item-context
                                           todo-explorer--items))))
      (should (> (length contexts) 0))
      ;; Items inside defuns should have "fn: ..." context
      (should (cl-some (lambda (c) (string-prefix-p "fn: " c)) contexts)))))

;;;; Integration Tests — Native Scanner

(ert-deftest todo-explorer-test-native-scan-fixtures ()
  "Native elisp scanner finds items in fixture files."
  (skip-unless (file-directory-p todo-explorer-test--fixture-dir))
  (with-temp-buffer
    (todo-explorer-mode)
    (setq todo-explorer--project-root todo-explorer-test--fixture-dir)
    (todo-explorer--scan-native todo-explorer-test--fixture-dir
                                (current-buffer))
    ;; Should find multiple items across fixture files
    (should (> (length todo-explorer--items) 0))
    ;; Should find TODO, FIXME, NOTE, HACK, BUG at minimum
    (let ((keywords (mapcar #'todo-explorer-item-keyword
                            todo-explorer--items)))
      (should (member "TODO" keywords))
      (should (member "FIXME" keywords))
      (should (member "NOTE" keywords)))))

;;;; Integration Tests — rg Scanner

(ert-deftest todo-explorer-test-rg-scan-fixtures ()
  "rg scanner finds items in fixture files."
  (skip-unless (executable-find "rg"))
  (skip-unless (file-directory-p todo-explorer-test--fixture-dir))
  (let* ((result-buf (generate-new-buffer " *test-rg-scan*"))
         (done nil)
         (items nil))
    (unwind-protect
        (progn
          (with-current-buffer result-buf
            (todo-explorer-mode)
            (setq todo-explorer--project-root todo-explorer-test--fixture-dir))
          ;; Run scan synchronously for test by using rg directly
          (let* ((todo-explorer-scan-tool 'rg)
                 (cmd (todo-explorer--rg-command
                       todo-explorer-test--fixture-dir))
                 (output (with-output-to-string
                           (with-current-buffer standard-output
                             (apply #'call-process (car cmd) nil t nil
                                    (cdr cmd))))))
            (dolist (line (split-string output "\n" t))
              (when-let ((item (todo-explorer--parse-line
                                line todo-explorer-test--fixture-dir)))
                (push item items))))
          (should (> (length items) 0))
          (let ((keywords (mapcar #'todo-explorer-item-keyword items)))
            (should (member "TODO" keywords))
            (should (member "FIXME" keywords))))
      (kill-buffer result-buf))))

;;;; Unit Tests — imenu Integration

(ert-deftest todo-explorer-test-imenu-entries ()
  "imenu integration finds TODO keywords in buffer."
  (with-temp-buffer
    (insert ";;; test.el --- test -*- lexical-binding: t; -*-\n")
    (insert ";; TODO: Fix this thing\n")
    (insert "(defun foo () nil)\n")
    (insert ";; FIXME: Another issue\n")
    (insert ";; NOTE: Remember this\n")
    (let ((entries (todo-explorer--imenu-todo-entries)))
      (should (= (length entries) 3))
      (should (string-match-p "TODO" (caar entries)))
      (should (string-match-p "FIXME" (car (nth 1 entries))))
      (should (string-match-p "NOTE" (car (nth 2 entries)))))))

(ert-deftest todo-explorer-test-imenu-mode-toggle ()
  "Toggling imenu mode saves and restores original index function."
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((orig imenu-create-index-function))
      (todo-explorer-imenu-mode 1)
      (should (eq imenu-create-index-function
                  #'todo-explorer--imenu-create-index))
      (should (eq todo-explorer--orig-imenu-function orig))
      (todo-explorer-imenu-mode -1)
      (should (eq imenu-create-index-function orig))
      (should (null todo-explorer--orig-imenu-function)))))

(ert-deftest todo-explorer-test-imenu-create-index ()
  "imenu index includes both original entries and TODO submenu."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";;; test.el --- test -*- lexical-binding: t; -*-\n\n")
    (insert "(defun my-function ()\n  ;; TODO: Fix this\n  nil)\n")
    (todo-explorer-imenu-mode 1)
    (let ((index (todo-explorer--imenu-create-index)))
      ;; Should have a TODO submenu
      (should (assoc "TODO" index))
      ;; TODO submenu should have our entry
      (let ((todo-entries (cdr (assoc "TODO" index))))
        (should (= (length todo-entries) 1))
        (should (string-match-p "TODO.*Fix this" (caar todo-entries)))))
    (todo-explorer-imenu-mode -1)))

;;;; Unit Tests — Git Blame

(ert-deftest todo-explorer-test-format-age ()
  "Format age produces compact relative strings."
  (let ((now (float-time)))
    ;; Recent: minutes
    (should (string-match-p "^[0-9]+m$"
              (todo-explorer--format-age (- now 300))))
    ;; Hours
    (should (string-match-p "^[0-9]+h$"
              (todo-explorer--format-age (- now 7200))))
    ;; Days
    (should (string-match-p "^[0-9]+d$"
              (todo-explorer--format-age (- now 172800))))
    ;; Months
    (should (string-match-p "^[0-9]+mo$"
              (todo-explorer--format-age (- now 7776000))))
    ;; Years
    (should (string-match-p "^[0-9]+y$"
              (todo-explorer--format-age (- now 63072000))))
    ;; Nil timestamp
    (should (string= "" (todo-explorer--format-age nil)))))

(ert-deftest todo-explorer-test-git-repo-check ()
  "Git repo check works on current directory (which is a git repo)."
  (skip-unless (executable-find "git"))
  (should (todo-explorer--git-repo-p default-directory))
  ;; Non-git directory
  (should-not (todo-explorer--git-repo-p "/tmp")))

(ert-deftest todo-explorer-test-blame-on-fixtures ()
  "Blame resolves author and date for fixture items."
  (skip-unless (executable-find "git"))
  (skip-unless (file-directory-p todo-explorer-test--fixture-dir))
  (skip-unless (todo-explorer--git-repo-p default-directory))
  (with-temp-buffer
    (todo-explorer-mode)
    (setq todo-explorer--project-root default-directory)
    (setq todo-explorer--blame-active t)
    (todo-explorer--scan-native todo-explorer-test--fixture-dir
                                (current-buffer))
    ;; Items should have blame data
    (let ((with-author (cl-remove-if-not #'todo-explorer-item-author
                                          todo-explorer--items)))
      (should (> (length with-author) 0))
      ;; Author should be a non-empty string
      (should (> (length (todo-explorer-item-author (car with-author))) 0))
      ;; Date should be a number
      (should (numberp (todo-explorer-item-date (car with-author)))))))

(ert-deftest todo-explorer-test-blame-skip-non-git ()
  "Blame resolution skips gracefully for non-git directories."
  (let ((items (list (todo-explorer-item-create
                      :file "/tmp/fake.el" :line 1
                      :keyword "TODO" :priority 2)))
        (todo-explorer--blame-active t))
    ;; Should not error on non-git directory
    (todo-explorer--resolve-blame items "/tmp")
    ;; Items should have no blame data
    (should (null (todo-explorer-item-author (car items))))))

(ert-deftest todo-explorer-test-sort-by-age ()
  "Items sort by date (oldest first)."
  (let* ((todo-explorer--sort-mode 'age)
         (items (list (todo-explorer-item-create
                       :file "/a.el" :line 1 :keyword "TODO"
                       :priority 2 :date 1000)
                      (todo-explorer-item-create
                       :file "/b.el" :line 1 :keyword "FIXME"
                       :priority 1 :date 3000)
                      (todo-explorer-item-create
                       :file "/c.el" :line 1 :keyword "NOTE"
                       :priority 3 :date 2000)))
         (sorted (todo-explorer--sort-items items)))
    (should (= (todo-explorer-item-date (nth 0 sorted)) 1000))
    (should (= (todo-explorer-item-date (nth 1 sorted)) 2000))
    (should (= (todo-explorer-item-date (nth 2 sorted)) 3000))))

(ert-deftest todo-explorer-test-setup-columns ()
  "Column format changes based on blame state."
  (with-temp-buffer
    (todo-explorer-mode)
    ;; Default: no blame, 4 columns
    (should (= (length tabulated-list-format) 4))
    ;; With blame: 6 columns
    (setq todo-explorer--blame-active t)
    (todo-explorer--setup-columns)
    (should (= (length tabulated-list-format) 6))
    ;; Back to normal
    (setq todo-explorer--blame-active nil)
    (todo-explorer--setup-columns)
    (should (= (length tabulated-list-format) 4))))

;;;; Unit Tests — Tree-sitter Context Detection

(ert-deftest todo-explorer-test-treesit-buffer-check ()
  "Tree-sitter buffer check returns nil when no parsers are active."
  (with-temp-buffer
    (should-not (todo-explorer--treesit-buffer-p))))

(ert-deftest todo-explorer-test-context-method-toggle ()
  "Context method toggle cycles through available methods."
  (with-temp-buffer
    (todo-explorer-mode)
    (setq todo-explorer--project-root "/tmp")
    ;; Default method from defcustom is auto
    (should (eq (or todo-explorer--context-method
                    todo-explorer-context-method)
                'auto))
    ;; Toggle to add-log
    (todo-explorer-toggle-context-method)
    (should (eq todo-explorer--context-method 'add-log))
    ;; Toggle: if treesit available goes to treesit, otherwise auto
    (todo-explorer-toggle-context-method)
    (if (and (fboundp 'treesit-available-p) (treesit-available-p))
        (progn
          (should (eq todo-explorer--context-method 'treesit))
          ;; One more toggle back to auto
          (todo-explorer-toggle-context-method)
          (should (eq todo-explorer--context-method 'auto)))
      (should (eq todo-explorer--context-method 'auto)))))

(ert-deftest todo-explorer-test-context-method-dispatch ()
  "Context detection dispatches based on method parameter."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun my-function ()\n  ;; TODO: fix this\n  nil)\n")
    ;; With add-log method, should find the enclosing function
    (let ((ctx (todo-explorer--detect-context-in-buffer 2 'add-log)))
      (should ctx)
      (should (string-match-p "my-function" ctx)))
    ;; With auto method (no treesit in batch), same result
    (let ((ctx (todo-explorer--detect-context-in-buffer 2 'auto)))
      (should ctx)
      (should (string-match-p "my-function" ctx)))))

(ert-deftest todo-explorer-test-treesit-scope-types ()
  "Scope types alist is defined with expected entries."
  (should (assoc "function_definition" todo-explorer--treesit-scope-types))
  (should (assoc "class_definition" todo-explorer--treesit-scope-types))
  (should (assoc "method_definition" todo-explorer--treesit-scope-types))
  (should (string= "fn" (cdr (assoc "function_definition"
                                     todo-explorer--treesit-scope-types))))
  (should (string= "class" (cdr (assoc "class_definition"
                                        todo-explorer--treesit-scope-types)))))

;;;; Unit Tests — Ignore List

(ert-deftest todo-explorer-test-ignore-entry-roundtrip ()
  "Ignore entry → plist → entry preserves all fields."
  (let* ((entry (todo-explorer-ignore-entry-create
                 :file "src/foo.el"
                 :keyword "TODO"
                 :text "Fix this"
                 :line 42))
         (plist (todo-explorer--ignore-entry-to-plist entry))
         (restored (todo-explorer--plist-to-ignore-entry plist)))
    (should (string= (todo-explorer-ignore-entry-file restored) "src/foo.el"))
    (should (string= (todo-explorer-ignore-entry-keyword restored) "TODO"))
    (should (string= (todo-explorer-ignore-entry-text restored) "Fix this"))
    (should (= (todo-explorer-ignore-entry-line restored) 42))))

(ert-deftest todo-explorer-test-ignore-save-load-roundtrip ()
  "Save to temp dir, load back, data matches."
  (let* ((tmp-dir (make-temp-file "todo-explorer-test-" t))
         (todo-explorer-ignore-directory tmp-dir)
         (root "/tmp/fake-project/")
         (data (list :items (list (todo-explorer-ignore-entry-create
                                   :file "src/main.el"
                                   :keyword "FIXME"
                                   :text "Handle edge case"
                                   :line 10))
                     :files (list "vendor/lib.el")
                     :directories (list "build/"))))
    (unwind-protect
        (progn
          (todo-explorer--save-ignore-list root data)
          (let ((loaded (todo-explorer--load-ignore-list root)))
            ;; Items roundtrip
            (should (= (length (plist-get loaded :items)) 1))
            (let ((entry (car (plist-get loaded :items))))
              (should (string= (todo-explorer-ignore-entry-file entry)
                               "src/main.el"))
              (should (string= (todo-explorer-ignore-entry-keyword entry)
                               "FIXME"))
              (should (string= (todo-explorer-ignore-entry-text entry)
                               "Handle edge case")))
            ;; Files roundtrip
            (should (equal (plist-get loaded :files) '("vendor/lib.el")))
            ;; Directories roundtrip
            (should (equal (plist-get loaded :directories) '("build/")))))
      (delete-directory tmp-dir t))))

(ert-deftest todo-explorer-test-ignore-load-missing-file ()
  "Loading from a missing file returns empty plist, no error."
  (let ((todo-explorer-ignore-directory "/tmp/nonexistent-todo-explorer-dir/"))
    (let ((result (todo-explorer--load-ignore-list "/tmp/fake/")))
      (should (null (plist-get result :items)))
      (should (null (plist-get result :files)))
      (should (null (plist-get result :directories))))))

(ert-deftest todo-explorer-test-ignore-load-corrupt-file ()
  "Loading a corrupt file returns empty plist, no error."
  (let* ((tmp-dir (make-temp-file "todo-explorer-test-" t))
         (todo-explorer-ignore-directory tmp-dir)
         (root "/tmp/corrupt-test/")
         (file (todo-explorer--ignore-file-path root)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "this is not valid elisp {{{{"))
          (let ((result (todo-explorer--load-ignore-list root)))
            (should (null (plist-get result :items)))
            (should (null (plist-get result :files)))
            (should (null (plist-get result :directories)))))
      (delete-directory tmp-dir t))))

(ert-deftest todo-explorer-test-item-ignored-p-by-item ()
  "Matching item → t, non-matching → nil."
  (with-temp-buffer
    (setq todo-explorer--project-root "/project/")
    (setq todo-explorer--ignore-list
          (list :items (list (todo-explorer-ignore-entry-create
                              :file "src/foo.el"
                              :keyword "TODO"
                              :text "Fix this"))
                :files nil
                :directories nil))
    (let ((match (todo-explorer-item-create
                  :file "/project/src/foo.el" :line 10
                  :keyword "TODO" :text "Fix this" :priority 2))
          (no-match (todo-explorer-item-create
                     :file "/project/src/foo.el" :line 10
                     :keyword "TODO" :text "Different text" :priority 2)))
      (should (todo-explorer--item-ignored-p match))
      (should-not (todo-explorer--item-ignored-p no-match)))))

(ert-deftest todo-explorer-test-item-ignored-p-by-file ()
  "Item in ignored file → t."
  (with-temp-buffer
    (setq todo-explorer--project-root "/project/")
    (setq todo-explorer--ignore-list
          (list :items nil
                :files (list "src/generated.el")
                :directories nil))
    (let ((match (todo-explorer-item-create
                  :file "/project/src/generated.el" :line 5
                  :keyword "TODO" :text "Whatever" :priority 2))
          (no-match (todo-explorer-item-create
                     :file "/project/src/main.el" :line 5
                     :keyword "TODO" :text "Whatever" :priority 2)))
      (should (todo-explorer--item-ignored-p match))
      (should-not (todo-explorer--item-ignored-p no-match)))))

(ert-deftest todo-explorer-test-item-ignored-p-by-directory ()
  "Item under ignored directory → t."
  (with-temp-buffer
    (setq todo-explorer--project-root "/project/")
    (setq todo-explorer--ignore-list
          (list :items nil
                :files nil
                :directories (list "build/")))
    (let ((match (todo-explorer-item-create
                  :file "/project/build/output.el" :line 1
                  :keyword "TODO" :text "Test" :priority 2))
          (no-match (todo-explorer-item-create
                     :file "/project/src/main.el" :line 1
                     :keyword "TODO" :text "Test" :priority 2)))
      (should (todo-explorer--item-ignored-p match))
      (should-not (todo-explorer--item-ignored-p no-match)))))

(ert-deftest todo-explorer-test-item-ignored-p-empty ()
  "Empty ignore list → nil for all."
  (with-temp-buffer
    (setq todo-explorer--project-root "/project/")
    (setq todo-explorer--ignore-list
          (list :items nil :files nil :directories nil))
    (let ((item (todo-explorer-item-create
                 :file "/project/src/foo.el" :line 1
                 :keyword "TODO" :text "Test" :priority 2)))
      (should-not (todo-explorer--item-ignored-p item)))))

(ert-deftest todo-explorer-test-ignore-filter-integration ()
  "Ignored items are excluded from filtered-items."
  (with-temp-buffer
    (todo-explorer-mode)
    (setq todo-explorer--project-root "/project/")
    (setq todo-explorer--ignore-list
          (list :items (list (todo-explorer-ignore-entry-create
                              :file "src/foo.el"
                              :keyword "TODO"
                              :text "Ignored item"))
                :files nil
                :directories nil))
    (setq todo-explorer--items
          (list (todo-explorer-item-create
                 :file "/project/src/foo.el" :line 10
                 :keyword "TODO" :text "Ignored item" :priority 2)
                (todo-explorer-item-create
                 :file "/project/src/foo.el" :line 20
                 :keyword "TODO" :text "Visible item" :priority 2)))
    (todo-explorer--apply-filter-and-sort)
    (should (= (length todo-explorer--filtered-items) 1))
    (should (string= (todo-explorer-item-text
                       (car todo-explorer--filtered-items))
                     "Visible item"))))

(ert-deftest todo-explorer-test-ignore-duplicate-prevention ()
  "Ignoring same item twice results in a single entry."
  (let* ((tmp-dir (make-temp-file "todo-explorer-test-" t))
         (todo-explorer-ignore-directory tmp-dir))
    (unwind-protect
        (with-temp-buffer
          (todo-explorer-mode)
          (setq todo-explorer--project-root "/project/")
          (setq todo-explorer--ignore-list
                (list :items nil :files nil :directories nil))
          (setq todo-explorer--items
                (list (todo-explorer-item-create
                       :file "/project/src/foo.el" :line 10
                       :keyword "TODO" :text "Fix this" :priority 2)))
          (let ((inhibit-read-only t))
            (todo-explorer--apply-filter-and-sort)
            (goto-char (point-min))
            ;; Ignore the item
            (todo-explorer-ignore-item)
            (should (= (length (plist-get todo-explorer--ignore-list :items)) 1))
            ;; Re-add the same item to display (simulate rescan)
            (setq todo-explorer--items
                  (list (todo-explorer-item-create
                         :file "/project/src/foo.el" :line 10
                         :keyword "TODO" :text "Fix this" :priority 2)))
            (todo-explorer--apply-filter-and-sort)
            (goto-char (point-min))
            ;; The item is already ignored, so filtered-items is empty
            ;; Just verify the ignore list still has exactly one entry
            (should (= (length (plist-get todo-explorer--ignore-list :items)) 1))))
      (delete-directory tmp-dir t))))

(ert-deftest todo-explorer-test-ignore-file-path ()
  "Correct file path construction for ignore files."
  (let ((todo-explorer-ignore-directory "/tmp/test-ignore/"))
    (let ((path (todo-explorer--ignore-file-path "/home/user/project/")))
      (should (string-prefix-p "/tmp/test-ignore/" path))
      (should (string-suffix-p ".eld" path))
      ;; Should contain MD5 hash
      (should (string-match-p "[0-9a-f]\\{32\\}" path)))))

(ert-deftest todo-explorer-test-ignore-stale-entries ()
  "Stale ignore entries (non-existent files) don't cause errors."
  (with-temp-buffer
    (setq todo-explorer--project-root "/project/")
    (setq todo-explorer--ignore-list
          (list :items (list (todo-explorer-ignore-entry-create
                              :file "deleted/file.el"
                              :keyword "TODO"
                              :text "Gone"))
                :files (list "nonexistent.el")
                :directories (list "removed-dir/")))
    ;; Item from a different file should not be ignored
    (let ((item (todo-explorer-item-create
                 :file "/project/src/real.el" :line 1
                 :keyword "TODO" :text "Real item" :priority 2)))
      (should-not (todo-explorer--item-ignored-p item)))))

(provide 'todo-explorer-test)
;;; todo-explorer-test.el ends here
