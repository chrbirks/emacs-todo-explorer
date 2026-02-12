;;; todo-explorer.el --- Project-wide TODO/FIXME/NOTE explorer -*- lexical-binding: t; -*-

;; Author: Christian Birk Sørensen
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience tools
;; URL: https://github.com/chrbirks/emacs-todo-explorer

;;; Commentary:

;; todo-explorer provides a dedicated, interactive buffer for browsing
;; all TODO, FIXME, NOTE, and similar keywords across your project.
;;
;; Usage:
;;   M-x todo-explorer              - scan current project
;;   M-x todo-explorer-current-file - scan current file only
;;
;; In the *todo-explorer* buffer:
;;   RET   - visit item at point
;;   o     - visit in other window
;;   j/k/n/p - next/previous item
;;   s       - cycle sort order
;;   f t     - filter: TODOs only
;;   f f     - filter: FIXMEs only
;;   f n     - filter: NOTEs only
;;   f p     - filter: by file path (regexp)
;;   f c     - filter: by context (regexp)
;;   f a     - filter: show all
;;   f 1/2/3 - filter by priority level
;;   /       - filter by description text (regexp)
;;   f w     - filter: by author (regexp, requires blame)
;;   F       - toggle follow mode (auto-peek)
;;   B       - toggle git blame (author + age columns)
;;   C       - cycle context method (auto/add-log/treesit)
;;   TAB     - expand/collapse context lines for item at point
;;   S-TAB   - expand/collapse all context lines
;;   i       - ignore item at point (persistent)
;;   I f     - ignore file of item at point
;;   I d     - ignore directory (with completion)
;;   I l     - manage ignore list
;;   I c     - clear all ignores
;;   g     - refresh (rescan)
;;   q     - quit
;;   ?     - help / command dispatch

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'tabulated-list)
(require 'seq)

(defvar hl-todo-keyword-faces)

;;;; Customization

(defgroup todo-explorer nil
  "Project-wide TODO/FIXME/NOTE explorer."
  :group 'tools
  :prefix "todo-explorer-")

(defcustom todo-explorer-keywords
  '("FIXME" "BUG" "TODO" "HACK" "NOTE" "XXX" "REVIEW" "OPTIMIZE")
  "Keywords to scan for."
  :type '(repeat string))

(defcustom todo-explorer-keyword-priorities
  '(("FIXME" . 1) ("BUG" . 1)
    ("TODO" . 2) ("HACK" . 2) ("XXX" . 2)
    ("NOTE" . 3) ("REVIEW" . 3) ("OPTIMIZE" . 3))
  "Alist mapping keywords to priority levels (1 = highest)."
  :type '(alist :key-type string :value-type integer))

(defcustom todo-explorer-keyword-faces
  '(("FIXME"    . todo-explorer-face-fixme)
    ("BUG"      . todo-explorer-face-fixme)
    ("TODO"     . todo-explorer-face-todo)
    ("HACK"     . todo-explorer-face-hack)
    ("XXX"      . todo-explorer-face-hack)
    ("NOTE"     . todo-explorer-face-note)
    ("REVIEW"   . todo-explorer-face-todo)
    ("OPTIMIZE" . todo-explorer-face-todo))
  "Alist mapping keywords to faces for display."
  :type '(alist :key-type string :value-type face))

(defcustom todo-explorer-scan-tool 'auto
  "Tool for scanning project files.
`auto' detects the best available tool."
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "ripgrep" rg)
                 (const :tag "grep" grep)
                 (const :tag "Emacs native" emacs)))

(defcustom todo-explorer-exclude-patterns
  '("*.min.js" "*.min.css" "*.map" "*.lock"
    "vendor/" "node_modules/" ".git/" "build/" "dist/")
  "Glob patterns to exclude from scanning.
Patterns ending in `/' are treated as directories."
  :type '(repeat string))

(defcustom todo-explorer-use-hl-todo-keywords nil
  "When non-nil and hl-todo is loaded, merge its keyword list."
  :type 'boolean)

(defcustom todo-explorer-project-root-function #'todo-explorer--project-root
  "Function to determine project root.
Should return a directory path or nil."
  :type 'function)

(defcustom todo-explorer-show-context t
  "When non-nil, detect and display semantic context for each item.
Context shows the enclosing function or scope (e.g. \"fn: my-func\")."
  :type 'boolean)

(defcustom todo-explorer-context-lines 3
  "Number of lines to show before and after each item when expanded.
Press TAB on an item to toggle context display."
  :type '(integer :tag "Lines of context"))

(defcustom todo-explorer-show-dispatch t
  "When non-nil, show the command dispatch on entering todo-explorer.
Requires the `transient' package (built-in since Emacs 29)."
  :type 'boolean)

(defcustom todo-explorer-display-action nil
  "Display action for the todo-explorer buffer.
When nil, display full-frame (delete other windows).
See Info node `(elisp) Choosing Window' for action format."
  :type '(choice (const :tag "Full frame (default)" nil)
                 (sexp :tag "Custom display action")))

(defcustom todo-explorer-show-blame nil
  "When non-nil, show git blame info (author and age) for each item.
Requires git.  Silently skipped if the project is not a git repository."
  :type 'boolean)

(defcustom todo-explorer-context-method 'auto
  "Method for detecting semantic context (enclosing function/scope).
`auto' uses tree-sitter when available, falling back to `add-log-current-defun'.
`add-log' always uses the traditional `add-log-current-defun' heuristic.
`treesit' prefers tree-sitter (Emacs 29+), with `add-log' fallback per file."
  :type '(choice (const :tag "Auto (tree-sitter when available)" auto)
                 (const :tag "add-log-current-defun" add-log)
                 (const :tag "Tree-sitter (Emacs 29+)" treesit)))

(defcustom todo-explorer-ignore-directory
  (locate-user-emacs-file "todo-explorer")
  "Directory for storing per-project ignore list files."
  :type 'directory)

;;;; Faces

(defface todo-explorer-face-fixme
  '((t :inherit error :weight bold))
  "Face for FIXME/BUG keywords.")

(defface todo-explorer-face-todo
  '((t :inherit warning :weight bold))
  "Face for TODO keywords.")

(defface todo-explorer-face-hack
  '((t :inherit warning))
  "Face for HACK/XXX keywords.")

(defface todo-explorer-face-note
  '((t :inherit success))
  "Face for NOTE keywords.")

(defface todo-explorer-face-context
  '((t :inherit shadow :slant italic))
  "Face for context labels.")

(defface todo-explorer-face-context-lines
  '((t :inherit shadow))
  "Face for expanded context lines around items.")

(defface todo-explorer-face-file
  '((t :inherit shadow))
  "Face for file paths.")

(defface todo-explorer-face-file-header
  '((t :inherit bold))
  "Face for file group header lines.")

(defface todo-explorer-face-line-number
  '((t :inherit shadow))
  "Face for line numbers.")

(defface todo-explorer-face-description
  '((t :inherit default))
  "Face for item description text.")

(defface todo-explorer-face-blame
  '((t :inherit shadow))
  "Face for git blame information (author and age).")

;;;; Fringe Indicators

(define-fringe-bitmap 'todo-explorer--priority-dot
  [#x18 #x3C #x3C #x18] nil nil 'center)

;;;; Data Structures

(cl-defstruct (todo-explorer-item
               (:constructor todo-explorer-item-create)
               (:copier nil))
  "A single TODO/FIXME/NOTE item."
  file
  line
  column
  keyword
  text
  priority
  context
  author
  date)

(cl-defstruct (todo-explorer-ignore-entry
               (:constructor todo-explorer-ignore-entry-create)
               (:copier nil))
  "An entry in the persistent ignore list."
  file
  keyword
  text
  line)

;;;; Internal Variables

(defvar todo-explorer--ignore-data (make-hash-table :test #'equal)
  "Global cache: project-root → ignore plist.")

(defvar-local todo-explorer--ignore-list nil
  "Buffer-local reference to current project's ignore data.
A plist with keys :items, :files, :directories.")

(defvar-local todo-explorer--items nil
  "Full list of scanned items.")

(defvar-local todo-explorer--filtered-items nil
  "Currently displayed (filtered) items.")

(defvar-local todo-explorer--active-filter nil
  "Active keyword filter string, or nil for all.")

(defvar-local todo-explorer--priority-filter nil
  "Active priority filter (integer 1-3), or nil for all.")

(defvar-local todo-explorer--text-filter nil
  "Regexp to match against item description text, or nil.")

(defvar-local todo-explorer--file-filter nil
  "Regexp to match against file paths, or nil.")

(defvar-local todo-explorer--context-filter nil
  "Regexp to match against context labels, or nil.")

(defvar-local todo-explorer--project-root nil
  "Root directory of the scanned project.")

(defvar-local todo-explorer--sort-mode 'file
  "Current sort mode: file, keyword, priority, context, or line.")

(defvar-local todo-explorer--process nil
  "Current scanning process, or nil.")

(defvar-local todo-explorer--partial-line ""
  "Incomplete line accumulated from process output.")

(defvar-local todo-explorer--pending-items nil
  "Items accumulated during an async scan.")

(defvar-local todo-explorer--scan-target nil
  "The target (file or directory) of the current/last scan.")

(defvar-local todo-explorer--expanded-items nil
  "Hash table of items whose context lines are currently shown.")

(defvar-local todo-explorer--scanner-name nil
  "Name of the scanner used for the current/last scan.")

(defvar-local todo-explorer--keyword-max-width nil
  "Cached max keyword width for badge formatting.")

(defvar-local todo-explorer--scan-generation 0
  "Counter incremented on each scan to detect stale sentinel callbacks.")

(defvar-local todo-explorer--follow-mode nil
  "Non-nil when follow mode is active (auto-peek on navigation).")

(defvar-local todo-explorer--follow-last-item nil
  "Last item peeked in follow mode, to avoid redundant peeks.")

(defvar-local todo-explorer--follow-window nil
  "Window created by follow mode for peeking, or nil.")

(defvar-local todo-explorer--blame-active nil
  "Non-nil when git blame columns are displayed.")

(defvar-local todo-explorer--author-filter nil
  "Regexp to match against author names, or nil.")

(defvar-local todo-explorer--context-method nil
  "Buffer-local context method override.
When nil, uses `todo-explorer-context-method'.")

;;;; Project Root Detection

(defun todo-explorer--project-root ()
  "Return project root via `project.el', or nil."
  (when-let ((project (project-current)))
    (project-root project)))

;;;; Ignore List Persistence

(defun todo-explorer--ignore-file-path (root)
  "Return the ignore file path for project ROOT."
  (expand-file-name (concat (md5 (expand-file-name root)) ".eld")
                    todo-explorer-ignore-directory))

(defun todo-explorer--ignore-entry-to-plist (entry)
  "Convert an `todo-explorer-ignore-entry' ENTRY to a plist."
  (list :file (todo-explorer-ignore-entry-file entry)
        :keyword (todo-explorer-ignore-entry-keyword entry)
        :text (todo-explorer-ignore-entry-text entry)
        :line (todo-explorer-ignore-entry-line entry)))

(defun todo-explorer--plist-to-ignore-entry (plist)
  "Convert a PLIST to a `todo-explorer-ignore-entry'."
  (todo-explorer-ignore-entry-create
   :file (plist-get plist :file)
   :keyword (plist-get plist :keyword)
   :text (plist-get plist :text)
   :line (plist-get plist :line)))

(defun todo-explorer--load-ignore-list (root)
  "Load the ignore list for project ROOT from disk.
Returns a plist (:items :files :directories).
Corrupt or missing files produce an empty list, never an error."
  (let ((file (todo-explorer--ignore-file-path root)))
    (if (file-readable-p file)
        (condition-case err
            (with-temp-buffer
              (insert-file-contents file)
              (let ((data (read (current-buffer))))
                (list :items (mapcar #'todo-explorer--plist-to-ignore-entry
                                     (plist-get data :items))
                      :files (plist-get data :files)
                      :directories (plist-get data :directories))))
          (error
           (message "todo-explorer: corrupt ignore file %s: %s" file err)
           (list :items nil :files nil :directories nil)))
      (list :items nil :files nil :directories nil))))

(defun todo-explorer--save-ignore-list (root data)
  "Save ignore DATA plist for project ROOT to disk.
Creates the storage directory if needed.  Updates the global cache."
  (let ((file (todo-explorer--ignore-file-path root))
        (dir todo-explorer-ignore-directory))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (let ((serialized (list :project (expand-file-name root)
                            :items (mapcar #'todo-explorer--ignore-entry-to-plist
                                           (plist-get data :items))
                            :files (plist-get data :files)
                            :directories (plist-get data :directories))))
      (with-temp-file file
        (pp serialized (current-buffer)))))
  (puthash (expand-file-name root) data todo-explorer--ignore-data))

(defun todo-explorer--ensure-ignore-list ()
  "Load the ignore list for the current project, caching as needed.
Sets the buffer-local `todo-explorer--ignore-list'."
  (when todo-explorer--project-root
    (let* ((root (expand-file-name todo-explorer--project-root))
           (cached (gethash root todo-explorer--ignore-data)))
      (setq todo-explorer--ignore-list
            (or cached
                (let ((data (todo-explorer--load-ignore-list root)))
                  (puthash root data todo-explorer--ignore-data)
                  data))))))

(defun todo-explorer--ignore-count ()
  "Return the total number of ignore entries in the current ignore list."
  (if todo-explorer--ignore-list
      (+ (length (plist-get todo-explorer--ignore-list :items))
         (length (plist-get todo-explorer--ignore-list :files))
         (length (plist-get todo-explorer--ignore-list :directories)))
    0))

;;;; Keywords & Faces

(defun todo-explorer--effective-keywords ()
  "Return the effective keyword list, merging hl-todo if configured."
  (if (and todo-explorer-use-hl-todo-keywords
           (bound-and-true-p hl-todo-keyword-faces))
      (cl-union todo-explorer-keywords
                (mapcar #'car hl-todo-keyword-faces)
                :test #'string=)
    todo-explorer-keywords))

(defun todo-explorer--keyword-priority (keyword)
  "Return priority for KEYWORD (1 = highest), default 2."
  (or (alist-get keyword todo-explorer-keyword-priorities nil nil #'string=)
      2))

(defun todo-explorer--keyword-face (keyword)
  "Return face for KEYWORD."
  (or (alist-get keyword todo-explorer-keyword-faces nil nil #'string=)
      'todo-explorer-face-todo))

;;;; Context Detection

(declare-function add-log-current-defun "add-log" ())

;; Tree-sitter declarations (Emacs 29+)
(declare-function treesit-available-p "treesit" ())
(declare-function treesit-node-at "treesit" (pos &optional parser-or-lang named))
(declare-function treesit-node-type "treesit" (node))
(declare-function treesit-node-parent "treesit" (node))
(declare-function treesit-node-child-by-field-name "treesit" (node field-name))
(declare-function treesit-node-text "treesit" (node &optional no-property))
(declare-function treesit-parser-list "treesit" (&optional buffer))

(defvar todo-explorer--treesit-scope-types
  '(;; Functions
    ("function_definition" . "fn")
    ("function_declaration" . "fn")
    ("function_item" . "fn")
    ("method_definition" . "method")
    ("method_declaration" . "method")
    ("arrow_function" . "fn")
    ("lexical_declaration" . nil)
    ;; Classes / structs
    ("class_definition" . "class")
    ("class_declaration" . "class")
    ("class_specifier" . "class")
    ("impl_item" . "impl")
    ("struct_specifier" . "struct")
    ("struct_item" . "struct")
    ("enum_item" . "enum")
    ("enum_specifier" . "enum")
    ("namespace_definition" . "ns")
    ;; Elisp tree-sitter
    ("defun" . "fn")
    ("defmacro" . "macro"))
  "Alist mapping tree-sitter node types to context label prefixes.
Only entries with a non-nil cdr are used for labeling; nil entries are skipped.")

(defun todo-explorer--treesit-buffer-p ()
  "Return non-nil if current buffer has active tree-sitter parsers."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (fboundp 'treesit-parser-list)
       (treesit-parser-list)))

(defun todo-explorer--detect-context-treesit (line-num)
  "Detect semantic context for LINE-NUM using tree-sitter.
Returns a string like \"class: Foo > method: bar\", or nil.
Walks parent nodes to find enclosing scopes."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-num))
    (back-to-indentation)
    (when-let ((node (treesit-node-at (point))))
      (let ((parts nil))
        (setq node (treesit-node-parent node))
        (while node
          (let* ((type (treesit-node-type node))
                 (entry (assoc type todo-explorer--treesit-scope-types))
                 (label-prefix (cdr entry)))
            (when (and entry label-prefix)
              (let ((name-node
                     (or (treesit-node-child-by-field-name node "name")
                         (treesit-node-child-by-field-name node "declarator"))))
                (when name-node
                  (push (format "%s: %s" label-prefix
                                (treesit-node-text name-node t))
                        parts)))))
          (setq node (treesit-node-parent node)))
        (when parts
          (mapconcat #'identity parts " > "))))))

(defun todo-explorer--detect-context-in-buffer (line-num &optional method)
  "Detect semantic context for LINE-NUM in current buffer.
Returns a string like \"fn: name\" or \"top-level\", or nil.
METHOD overrides `todo-explorer-context-method' when non-nil."
  (let* ((method (or method todo-explorer-context-method))
         (use-treesit (and (memq method '(auto treesit))
                           (todo-explorer--treesit-buffer-p))))
    (or (when use-treesit
          (ignore-errors (todo-explorer--detect-context-treesit line-num)))
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- line-num))
          (or (when-let ((fn (ignore-errors (add-log-current-defun))))
                (concat "fn: " fn))
              (cond
               ((<= line-num 3) "file header")
               ((zerop (current-indentation)) "top-level")))))))

(defun todo-explorer--resolve-contexts (items)
  "Resolve semantic context for all ITEMS.
Groups items by file and opens each file once to minimize I/O."
  (when todo-explorer-show-context
    (let ((method (or todo-explorer--context-method
                      todo-explorer-context-method))
          (by-file (make-hash-table :test #'equal)))
      (dolist (item items)
        (push item (gethash (todo-explorer-item-file item) by-file)))
      (maphash
       (lambda (file file-items)
         (when (file-readable-p file)
           (with-temp-buffer
             (let ((buffer-file-name file))
               (insert-file-contents file)
               (delay-mode-hooks (set-auto-mode))
               (dolist (item file-items)
                 (setf (todo-explorer-item-context item)
                       (todo-explorer--detect-context-in-buffer
                        (todo-explorer-item-line item) method)))))))
       by-file))))

;;;; Git Blame

(defun todo-explorer--git-repo-p (root)
  "Return non-nil if ROOT is inside a git work tree."
  (and root
       (executable-find "git")
       (zerop (call-process "git" nil nil nil
                            "-C" (expand-file-name root)
                            "rev-parse" "--is-inside-work-tree"))))

(defun todo-explorer--format-age (timestamp)
  "Format TIMESTAMP (unix seconds) as a compact relative age string."
  (if (null timestamp)
      ""
    (let ((diff (max 0 (- (float-time) timestamp))))
      (cond
       ((< diff 3600)    (format "%dm"  (truncate (/ diff 60))))
       ((< diff 86400)   (format "%dh"  (truncate (/ diff 3600))))
       ((< diff 604800)  (format "%dd"  (truncate (/ diff 86400))))
       ((< diff 2592000) (format "%dw"  (truncate (/ diff 604800))))
       ((< diff 31536000)(format "%dmo" (truncate (/ diff 2592000))))
       (t                (format "%dy"  (truncate (/ diff 31536000))))))))

(defun todo-explorer--blame-file (file items root)
  "Run git blame on FILE for ITEMS, populating author and date fields.
ROOT is the project root for running git."
  (let ((args (list "blame" "--porcelain"))
        (item-by-line (make-hash-table :test #'eql)))
    (dolist (item items)
      (let ((line (todo-explorer-item-line item)))
        (puthash line item item-by-line)
        (setq args (append args (list "-L" (format "%d,%d" line line))))))
    (setq args (append args (list "--" (file-relative-name file root))))
    (with-temp-buffer
      (let ((default-directory (expand-file-name root)))
        (when (zerop (apply #'call-process "git" nil t nil args))
          (goto-char (point-min))
          (let ((commits (make-hash-table :test #'equal)))
            (while (re-search-forward
                    "^\\([0-9a-f]+\\) [0-9]+ \\([0-9]+\\)" nil t)
              (let ((sha (match-string 1))
                    (final-line (string-to-number (match-string 2)))
                    author author-time)
                (forward-line 1)
                (while (and (not (eobp)) (not (eq (char-after) ?\t)))
                  (cond
                   ((looking-at "author \\(.+\\)")
                    (setq author (match-string 1)))
                   ((looking-at "author-time \\([0-9]+\\)")
                    (setq author-time
                          (string-to-number (match-string 1)))))
                  (forward-line 1))
                ;; Skip content line (tab-prefixed)
                (when (and (not (eobp)) (eq (char-after) ?\t))
                  (forward-line 1))
                ;; Cache commit data (first occurrence has headers)
                (when author
                  (puthash sha (cons author author-time) commits))
                ;; Apply to item
                (let ((commit (gethash sha commits)))
                  (when commit
                    (let ((item (gethash final-line item-by-line)))
                      (when item
                        (setf (todo-explorer-item-author item) (car commit))
                        (setf (todo-explorer-item-date item)
                              (cdr commit))))))))))))))

(defun todo-explorer--resolve-blame (items root)
  "Resolve git blame info for ITEMS in project ROOT.
Skips silently if git is unavailable or ROOT is not a git repository."
  (when (and todo-explorer--blame-active
             (todo-explorer--git-repo-p root))
    (let ((by-file (make-hash-table :test #'equal)))
      (dolist (item items)
        (push item (gethash (todo-explorer-item-file item) by-file)))
      (maphash
       (lambda (file file-items)
         (when (file-readable-p file)
           (condition-case nil
               (todo-explorer--blame-file file file-items root)
             (error nil))))
       by-file))))

;;;; Regexp Construction

(defun todo-explorer--build-scanner-regexp ()
  "Build a POSIX ERE regexp for rg and grep keyword scanning."
  (concat "\\b("
          (mapconcat #'regexp-quote
                     (todo-explorer--effective-keywords) "|")
          ")\\b"))

(defun todo-explorer--build-elisp-regexp ()
  "Build an Emacs regexp for scanning keywords."
  (concat "\\_<" (regexp-opt (todo-explorer--effective-keywords) t) "\\_>"))

;;;; Scanner Detection

(defun todo-explorer--detect-scanner ()
  "Return the best available scanner symbol."
  (if (eq todo-explorer-scan-tool 'auto)
      (cond
       ((executable-find "rg") 'rg)
       ((executable-find "grep") 'grep)
       (t 'emacs))
    todo-explorer-scan-tool))

;;;; Scanner Commands

(defun todo-explorer--rg-command (target)
  "Build rg command list to scan TARGET (directory or file)."
  (let ((cmd (list "rg" "--vimgrep" "--no-heading" "--color" "never"
                   "--case-sensitive"
                   "-e" (todo-explorer--build-scanner-regexp))))
    (when (file-directory-p target)
      (dolist (pat todo-explorer-exclude-patterns)
        (setq cmd (append cmd (list "--glob" (concat "!" pat))))))
    (append cmd (list (expand-file-name target)))))

(defun todo-explorer--grep-command (target)
  "Build grep command list to scan TARGET (directory or file)."
  (let* ((is-dir (file-directory-p target))
         (cmd (if is-dir
                  (list "grep" "-r" "-n" "-I" "-E"
                        (todo-explorer--build-scanner-regexp))
                (list "grep" "-n" "-E"
                      (todo-explorer--build-scanner-regexp)))))
    (when is-dir
      (dolist (pat todo-explorer-exclude-patterns)
        (if (string-suffix-p "/" pat)
            (setq cmd (append cmd (list "--exclude-dir"
                                        (substring pat 0 (1- (length pat))))))
          (setq cmd (append cmd (list "--exclude" pat))))))
    (append cmd (list (expand-file-name target)))))

;;;; Line Parsing

(defun todo-explorer--parse-line (line root)
  "Parse a scanner output LINE into a `todo-explorer-item'.
ROOT is the project root for resolving relative paths.
Returns nil if LINE cannot be parsed."
  (when (string-match
         "^\\(.+?\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)?\\(.*\\)$"
         line)
    (let* ((file-raw (match-string 1 line))
           (line-num (string-to-number (match-string 2 line)))
           (col (when (match-string 3 line)
                  (string-to-number (match-string 3 line))))
           (context (match-string 4 line))
           (file (if (file-name-absolute-p file-raw)
                     file-raw
                   (expand-file-name file-raw root)))
           (kw-regexp (concat "\\b\\("
                              (mapconcat #'regexp-quote
                                         (todo-explorer--effective-keywords)
                                         "\\|")
                              "\\)\\b"))
           keyword text)
      (when (let ((case-fold-search nil))
              (string-match kw-regexp context))
        (setq keyword (match-string 1 context))
        (setq text (string-trim (substring context (match-end 0))))
        ;; Strip leading colon/dash from text
        (when (string-match "\\`[: -]+" text)
          (setq text (string-trim (substring text (match-end 0)))))
        (todo-explorer-item-create
         :file file
         :line line-num
         :column col
         :keyword keyword
         :text text
         :priority (todo-explorer--keyword-priority keyword))))))

;;;; Async Scanning

(defun todo-explorer--kill-process ()
  "Kill any running scan process in the current buffer."
  (when (and todo-explorer--process
             (process-live-p todo-explorer--process))
    (delete-process todo-explorer--process))
  (setq todo-explorer--process nil))

(defun todo-explorer--scan (target result-buffer)
  "Scan TARGET asynchronously, populating RESULT-BUFFER when done."
  (with-current-buffer result-buffer
    (todo-explorer--kill-process)
    (setq todo-explorer--pending-items nil)
    (setq todo-explorer--partial-line "")
    (setq todo-explorer--expanded-items nil)
    (setq todo-explorer--keyword-max-width nil)
    (cl-incf todo-explorer--scan-generation)
    (let* ((scanner (todo-explorer--detect-scanner))
           (root todo-explorer--project-root)
           (generation todo-explorer--scan-generation))
      (setq todo-explorer--scanner-name (symbol-name scanner))
      (setq mode-line-process
            (format " Scanning with %s..." todo-explorer--scanner-name))
      (force-mode-line-update)
      (if (eq scanner 'emacs)
          (todo-explorer--scan-native target result-buffer)
        (let* ((cmd (pcase scanner
                      ('rg (todo-explorer--rg-command target))
                      ('grep (todo-explorer--grep-command target))))
               (proc-buf (generate-new-buffer " *todo-explorer-proc*"))
               (proc (make-process
                      :name "todo-explorer-scan"
                      :buffer proc-buf
                      :command cmd
                      :noquery t
                      :connection-type 'pipe
                      :sentinel (todo-explorer--make-sentinel
                                 result-buffer root generation)
                      :filter (todo-explorer--make-filter
                               result-buffer root))))
          (setq todo-explorer--process proc))))))

(defun todo-explorer--make-filter (result-buffer root)
  "Return a process filter that parses lines into RESULT-BUFFER.
ROOT is used for resolving relative file paths."
  (lambda (_proc output)
    (when (buffer-live-p result-buffer)
      (with-current-buffer result-buffer
        (let* ((combined (concat todo-explorer--partial-line output))
               (lines (split-string combined "\n"))
               (complete (butlast lines)))
          (setq todo-explorer--partial-line (car (last lines)))
          (dolist (line complete)
            (when-let ((item (todo-explorer--parse-line line root)))
              (push item todo-explorer--pending-items))))))))

(defun todo-explorer--make-sentinel (result-buffer root generation)
  "Return a process sentinel that finalizes results in RESULT-BUFFER.
ROOT is the project root.  GENERATION prevents stale callbacks."
  (lambda (proc _event)
    (when (buffer-live-p result-buffer)
      (with-current-buffer result-buffer
        ;; Only process results if this is still the current scan
        (when (= generation todo-explorer--scan-generation)
          (unless (string-empty-p todo-explorer--partial-line)
            (when-let ((item (todo-explorer--parse-line
                              todo-explorer--partial-line root)))
              (push item todo-explorer--pending-items)))
          (setq todo-explorer--partial-line "")
          (setq todo-explorer--items (nreverse todo-explorer--pending-items))
          (setq todo-explorer--pending-items nil)
          (setq todo-explorer--process nil)
          (todo-explorer--resolve-contexts todo-explorer--items)
          (todo-explorer--resolve-blame todo-explorer--items
                                        todo-explorer--project-root)
          (todo-explorer--apply-filter-and-sort))))
    ;; Clean up process buffer regardless of generation
    (when-let ((buf (process-buffer proc)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

;;;; Native Elisp Scanner

(defun todo-explorer--scan-native (target result-buffer)
  "Scan TARGET using native Emacs Lisp, populating RESULT-BUFFER."
  (with-current-buffer result-buffer
    (let ((regexp (todo-explorer--build-elisp-regexp))
          (files (if (file-directory-p target)
                     (or (when-let ((proj (project-current nil target)))
                           (project-files proj))
                         (directory-files-recursively target ""))
                   (list target)))
          items)
      (dolist (file files)
        (when (and (file-readable-p file)
                   (not (file-directory-p file))
                   (let ((size (file-attribute-size (file-attributes file))))
                     (and size (< size 1048576))))
          (with-temp-buffer
            (insert-file-contents file)
            ;; Skip binary files (null bytes in first 512 chars)
            (unless (string-match-p "\0"
                      (buffer-substring-no-properties
                       (point-min) (min (point-max) (+ (point-min) 512))))
              (goto-char (point-min))
              (let ((case-fold-search nil))
              (while (re-search-forward regexp nil t)
                (let* ((keyword (match-string 1))
                       (line-num (line-number-at-pos (match-beginning 0)))
                       (col (- (match-beginning 0) (line-beginning-position)))
                       (line-end (line-end-position))
                       (text-after (string-trim
                                    (buffer-substring-no-properties
                                     (match-end 0) line-end))))
                  ;; Strip leading colon/dash
                  (when (string-match "\\`[: -]+" text-after)
                    (setq text-after (string-trim
                                      (substring text-after (match-end 0)))))
                  (push (todo-explorer-item-create
                         :file (expand-file-name file)
                         :line line-num
                         :column col
                         :keyword keyword
                         :text text-after
                         :priority (todo-explorer--keyword-priority keyword))
                        items))))))))
      (setq todo-explorer--items (nreverse items))
      (todo-explorer--resolve-contexts todo-explorer--items)
      (todo-explorer--resolve-blame todo-explorer--items
                                    todo-explorer--project-root)
      (todo-explorer--apply-filter-and-sort))))

;;;; Sorting

(defun todo-explorer--sort-items (items)
  "Sort ITEMS according to `todo-explorer--sort-mode'."
  (let ((sorted (copy-sequence items)))
    (pcase todo-explorer--sort-mode
      ('file
       (sort sorted (lambda (a b)
                      (let ((fa (todo-explorer-item-file a))
                            (fb (todo-explorer-item-file b)))
                        (if (string= fa fb)
                            (< (todo-explorer-item-line a)
                               (todo-explorer-item-line b))
                          (string< fa fb))))))
      ('keyword
       (sort sorted (lambda (a b)
                      (let ((ka (todo-explorer-item-keyword a))
                            (kb (todo-explorer-item-keyword b)))
                        (if (string= ka kb)
                            (string< (todo-explorer-item-file a)
                                     (todo-explorer-item-file b))
                          (string< ka kb))))))
      ('priority
       (sort sorted (lambda (a b)
                      (let ((pa (todo-explorer-item-priority a))
                            (pb (todo-explorer-item-priority b)))
                        (if (= pa pb)
                            (string< (todo-explorer-item-file a)
                                     (todo-explorer-item-file b))
                          (< pa pb))))))
      ('context
       (sort sorted (lambda (a b)
                      (let ((ca (or (todo-explorer-item-context a) ""))
                            (cb (or (todo-explorer-item-context b) "")))
                        (if (string= ca cb)
                            (string< (todo-explorer-item-file a)
                                     (todo-explorer-item-file b))
                          (string< ca cb))))))
      ('age
       (sort sorted (lambda (a b)
                      (let ((da (or (todo-explorer-item-date a) 0))
                            (db (or (todo-explorer-item-date b) 0)))
                        (if (= da db)
                            (string< (todo-explorer-item-file a)
                                     (todo-explorer-item-file b))
                          (< da db)))))))))

;;;; Ignore Matching

(defun todo-explorer--item-ignored-p (item)
  "Return non-nil if ITEM should be hidden by the ignore list."
  (when todo-explorer--ignore-list
    (let ((rel-file (file-relative-name (todo-explorer-item-file item)
                                        todo-explorer--project-root)))
      (or
       ;; File match
       (member rel-file (plist-get todo-explorer--ignore-list :files))
       ;; Directory match
       (cl-some (lambda (dir)
                  (string-prefix-p dir rel-file))
                (plist-get todo-explorer--ignore-list :directories))
       ;; Item match: (file keyword text)
       (cl-some (lambda (entry)
                  (and (string= rel-file
                                (todo-explorer-ignore-entry-file entry))
                       (string= (todo-explorer-item-keyword item)
                                (todo-explorer-ignore-entry-keyword entry))
                       (string= (todo-explorer-item-text item)
                                (todo-explorer-ignore-entry-text entry))))
                (plist-get todo-explorer--ignore-list :items))))))

;;;; Filtering

(defun todo-explorer--apply-filter-and-sort ()
  "Apply current filter and sort, then refresh the display."
  (let ((items todo-explorer--items))
    (when todo-explorer--ignore-list
      (setq items (cl-remove-if #'todo-explorer--item-ignored-p items)))
    (when todo-explorer--active-filter
      (setq items (cl-remove-if-not
                   (lambda (item)
                     (string= (todo-explorer-item-keyword item)
                              todo-explorer--active-filter))
                   items)))
    (when todo-explorer--priority-filter
      (setq items (cl-remove-if-not
                   (lambda (item)
                     (<= (todo-explorer-item-priority item)
                         todo-explorer--priority-filter))
                   items)))
    (when todo-explorer--text-filter
      (setq items (cl-remove-if-not
                   (lambda (item)
                     (string-match-p todo-explorer--text-filter
                                     (or (todo-explorer-item-text item) "")))
                   items)))
    (when todo-explorer--file-filter
      (setq items (cl-remove-if-not
                   (lambda (item)
                     (string-match-p todo-explorer--file-filter
                                     (todo-explorer--relative-path
                                      (todo-explorer-item-file item))))
                   items)))
    (when todo-explorer--context-filter
      (setq items (cl-remove-if-not
                   (lambda (item)
                     (string-match-p todo-explorer--context-filter
                                     (or (todo-explorer-item-context item) "")))
                   items)))
    (when todo-explorer--author-filter
      (setq items (cl-remove-if-not
                   (lambda (item)
                     (string-match-p todo-explorer--author-filter
                                     (or (todo-explorer-item-author item) "")))
                   items)))
    (setq todo-explorer--filtered-items (todo-explorer--sort-items items)))
  (todo-explorer--refresh-display)
  (todo-explorer--update-header))

;;;; Display

(defun todo-explorer--relative-path (file)
  "Return FILE path relative to project root."
  (if todo-explorer--project-root
      (file-relative-name file todo-explorer--project-root)
    (abbreviate-file-name file)))

(defun todo-explorer--keyword-badge (keyword face)
  "Format KEYWORD as a fixed-width bracketed badge with FACE."
  (let* ((width (or todo-explorer--keyword-max-width
                    (setq todo-explorer--keyword-max-width
                          (apply #'max (mapcar #'length
                                               (todo-explorer--effective-keywords))))))
         (padded (format (format "%%-%ds" width) keyword)))
    (propertize (concat "[" padded "]") 'face face)))

(defun todo-explorer--make-entry (item)
  "Convert ITEM to a `tabulated-list' entry."
  (let* ((keyword (todo-explorer-item-keyword item))
         (face (todo-explorer--keyword-face keyword))
         (file (todo-explorer--relative-path (todo-explorer-item-file item)))
         (line-str (number-to-string (todo-explorer-item-line item)))
         (location (concat (propertize file 'face 'todo-explorer-face-file)
                           ":"
                           (propertize line-str 'face 'todo-explorer-face-line-number)))
         (text (or (todo-explorer-item-text item) ""))
         (fringe (propertize
                  " " 'display
                  `(left-fringe todo-explorer--priority-dot ,face)))
         (badge (concat fringe (todo-explorer--keyword-badge keyword face)))
         (ctx (propertize (or (todo-explorer-item-context item) "")
                          'face 'todo-explorer-face-context))
         (desc (propertize text 'face 'todo-explorer-face-description)))
    (if todo-explorer--blame-active
        (let ((age (propertize (todo-explorer--format-age
                                (todo-explorer-item-date item))
                               'face 'todo-explorer-face-blame))
              (author (propertize (or (todo-explorer-item-author item) "")
                                  'face 'todo-explorer-face-blame)))
          (list item (vector badge location desc ctx age author)))
      (list item (vector badge location desc ctx)))))

(defun todo-explorer--refresh-display ()
  "Populate the tabulated list from `todo-explorer--filtered-items'."
  (todo-explorer--remove-context-overlays)
  (todo-explorer--remove-file-headers)
  (let ((entries (mapcar #'todo-explorer--make-entry
                         todo-explorer--filtered-items)))
    (when (eq todo-explorer--sort-mode 'file)
      (dolist (entry entries)
        (let* ((item (car entry))
               (line-str (number-to-string (todo-explorer-item-line item)))
               (short-loc (propertize line-str
                                      'face 'todo-explorer-face-line-number)))
          (aset (cadr entry) 1 short-loc))))
    (setq tabulated-list-entries entries))
  (tabulated-list-print t)
  (goto-char (point-min))
  (if (null tabulated-list-entries)
      (let ((msg (if (or todo-explorer--active-filter
                         todo-explorer--priority-filter
                         todo-explorer--text-filter
                         todo-explorer--file-filter
                         todo-explorer--context-filter
                         todo-explorer--author-filter)
                     "No matching items. Press 'f a' to clear filters."
                   "No TODO items found. Press 'g' to rescan or '?' for help.")))
        (insert (propertize (concat "\n  " msg) 'face 'shadow)))
    (todo-explorer--insert-file-group-headers)
    (todo-explorer--restore-expansions)))

(defun todo-explorer--keyword-counts (items)
  "Return an alist of (KEYWORD . COUNT) for ITEMS, sorted by priority."
  (let ((counts (make-hash-table :test #'equal))
        result)
    (dolist (item items)
      (let ((kw (todo-explorer-item-keyword item)))
        (puthash kw (1+ (gethash kw counts 0)) counts)))
    (maphash (lambda (kw n) (push (cons kw n) result)) counts)
    (sort result
          (lambda (a b)
            (< (todo-explorer--keyword-priority (car a))
               (todo-explorer--keyword-priority (car b)))))))

(defun todo-explorer--format-keyword-counts (items)
  "Return a propertized string of per-keyword counts for ITEMS."
  (mapconcat
   (lambda (pair)
     (let ((kw (car pair))
           (n (cdr pair)))
       (propertize (format "%d %s" n kw)
                   'face (todo-explorer--keyword-face kw))))
   (todo-explorer--keyword-counts items)
   "  "))

(defun todo-explorer--update-header ()
  "Update mode-line with current state.
Column headers are managed by `tabulated-list-init-header'."
  (let* ((scanner-name (or todo-explorer--scanner-name "?"))
         (items (if todo-explorer--active-filter
                    todo-explorer--filtered-items
                  todo-explorer--items))
         (n-shown (length todo-explorer--filtered-items))
         (n-total (length todo-explorer--items))
         (n-files (length (delete-dups
                           (mapcar #'todo-explorer-item-file
                                   (copy-sequence
                                    todo-explorer--filtered-items)))))
         (counts-str (todo-explorer--format-keyword-counts items))
         (filters (concat
                   (if todo-explorer--active-filter
                       (format " kw:%s" todo-explorer--active-filter) "")
                   (if todo-explorer--priority-filter
                       (format " p<=%d" todo-explorer--priority-filter) "")
                   (if todo-explorer--text-filter
                       (format " /%s/" todo-explorer--text-filter) "")
                   (if todo-explorer--file-filter
                       (format " file:%s" todo-explorer--file-filter) "")
                   (if todo-explorer--context-filter
                       (format " ctx:%s" todo-explorer--context-filter) "")
                   (if todo-explorer--author-filter
                       (format " @%s" todo-explorer--author-filter) "")
                   (if todo-explorer--blame-active " blame" "")
                   (let ((ign (todo-explorer--ignore-count)))
                     (if (> ign 0) (format " ign:%d" ign) "")))))
    (setq mode-line-process
          (concat (format " %d%s in %d files"
                          n-shown
                          (if todo-explorer--active-filter
                              (format "/%d" n-total) "")
                          n-files)
                  " " counts-str
                  (format " [%s|%s|%s%s]" scanner-name
                          todo-explorer--sort-mode
                          (or todo-explorer--context-method
                              todo-explorer-context-method)
                          filters)))
    (force-mode-line-update)))

;;;; Context Line Expansion

(defun todo-explorer--fetch-context-lines (file line-num n)
  "Fetch N lines before and after LINE-NUM from FILE.
Returns a formatted string with line numbers and syntax highlighting, or nil."
  (when (file-readable-p file)
    (let* ((visiting-buf (find-buffer-visiting file))
           (buf (or visiting-buf (generate-new-buffer " *todo-explorer-hl*"))))
      (unwind-protect
          (with-current-buffer buf
            (save-excursion
              (unless visiting-buf
                (let ((buffer-file-name file))
                  (insert-file-contents file)
                  (delay-mode-hooks (set-auto-mode))
                  (font-lock-mode 1)))
              ;; Ensure region is fontified (JIT font-lock may not have
              ;; reached off-screen lines in visiting buffers)
              (goto-char (point-min))
              (forward-line (1- (max 1 (- line-num n))))
              (let ((region-start (point)))
                (forward-line (1+ (* 2 n)))
                (font-lock-ensure region-start (point)))
              ;; Extract lines with syntax highlighting
              (let* ((start-line (max 1 (- line-num n)))
                     (end-line (+ line-num n))
                     (num-width (length (number-to-string end-line)))
                     (prefix-fmt (format "      %%s %%%dd: " num-width))
                     (lines nil))
                (goto-char (point-min))
                (forward-line (1- start-line))
                (cl-loop for lnum from start-line to end-line
                         while (not (eobp))
                         do (let* ((code (buffer-substring
                                          (line-beginning-position)
                                          (line-end-position)))
                                   (marker (if (= lnum line-num) ">" " "))
                                   (prefix (propertize
                                            (format prefix-fmt marker lnum)
                                            'face 'todo-explorer-face-context-lines)))
                              (push (concat prefix code) lines))
                         (forward-line 1))
                (mapconcat #'identity (nreverse lines) "\n"))))
        (unless visiting-buf
          (kill-buffer buf))))))

(defun todo-explorer--show-context-overlay (item)
  "Create an overlay showing context lines for ITEM at current line."
  (let* ((file (todo-explorer-item-file item))
         (line-num (todo-explorer-item-line item))
         (context-text (todo-explorer--fetch-context-lines
                        file line-num todo-explorer-context-lines)))
    (when context-text
      (let* ((next-bol (1+ (line-end-position)))
             (ov (make-overlay next-bol next-bol)))
        (overlay-put ov 'todo-explorer-context-ov t)
        (overlay-put ov 'before-string (concat context-text "\n"))))))

(defun todo-explorer--remove-context-overlays ()
  "Remove all context line overlays from the buffer."
  (remove-overlays (point-min) (point-max) 'todo-explorer-context-ov t))

;;;; File Group Headers

(defun todo-explorer--remove-file-headers ()
  "Remove all file group header overlays from the buffer."
  (remove-overlays (point-min) (point-max) 'todo-explorer-file-header t))

(defun todo-explorer--insert-file-group-headers ()
  "Insert file group header overlays when sorted by file."
  (when (eq todo-explorer--sort-mode 'file)
    (let ((file-counts (make-hash-table :test #'equal)))
      (dolist (item todo-explorer--filtered-items)
        (let ((f (todo-explorer--relative-path (todo-explorer-item-file item))))
          (puthash f (1+ (gethash f file-counts 0)) file-counts)))
      (save-excursion
        (goto-char (point-min))
        (let ((current-file nil)
              (first t)
              (rule (propertize (make-string (window-body-width) ?─)
                                'face 'shadow)))
          (while (not (eobp))
            (when-let ((item (tabulated-list-get-id)))
              (when (todo-explorer-item-p item)
                (let ((file (todo-explorer--relative-path
                             (todo-explorer-item-file item))))
                  (unless (equal file current-file)
                    (setq current-file file)
                    (let* ((count (gethash file file-counts 0))
                           (label (format " %s (%d %s)\n" file count
                                          (if (= count 1) "item" "items")))
                           (ov (make-overlay (line-beginning-position)
                                             (line-beginning-position))))
                      (overlay-put ov 'todo-explorer-file-header t)
                      (overlay-put ov 'before-string
                                   (concat
                                    (if first "" "\n")
                                    rule "\n"
                                    (propertize label
                                                'face 'todo-explorer-face-file-header)
                                    rule "\n")))
                    (setq first nil)))))
            (forward-line 1)))))))

(defun todo-explorer--restore-expansions ()
  "Re-create context overlays for all expanded items after a refresh."
  (when (and todo-explorer--expanded-items
             (> (hash-table-count todo-explorer--expanded-items) 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let ((item (tabulated-list-get-id)))
          (when (gethash item todo-explorer--expanded-items)
            (todo-explorer--show-context-overlay item)))
        (forward-line 1)))))

(defun todo-explorer-toggle-context ()
  "Toggle display of surrounding code lines for the item at point."
  (interactive)
  (when-let ((item (todo-explorer--item-at-point)))
    (unless todo-explorer--expanded-items
      (setq todo-explorer--expanded-items (make-hash-table :test #'eq)))
    (if (gethash item todo-explorer--expanded-items)
        (progn
          (remhash item todo-explorer--expanded-items)
          (dolist (ov (overlays-in (line-beginning-position)
                                   (min (point-max) (+ 2 (line-end-position)))))
            (when (overlay-get ov 'todo-explorer-context-ov)
              (delete-overlay ov))))
      (puthash item t todo-explorer--expanded-items)
      (todo-explorer--show-context-overlay item))))

(defun todo-explorer-toggle-context-all ()
  "Expand all items if any are collapsed, otherwise collapse all."
  (interactive)
  (unless todo-explorer--expanded-items
    (setq todo-explorer--expanded-items (make-hash-table :test #'eq)))
  (let ((all-expanded (= (hash-table-count todo-explorer--expanded-items)
                         (length todo-explorer--filtered-items))))
    (if all-expanded
        (progn
          (clrhash todo-explorer--expanded-items)
          (todo-explorer--remove-context-overlays))
      (todo-explorer--remove-context-overlays)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when-let ((item (tabulated-list-get-id)))
            (puthash item t todo-explorer--expanded-items)
            (todo-explorer--show-context-overlay item))
          (forward-line 1))))))

;;;; Navigation

(defun todo-explorer--item-at-point ()
  "Return the `todo-explorer-item' at point, or nil."
  (tabulated-list-get-id))

(defun todo-explorer-visit-item ()
  "Visit the source location of the item at point."
  (interactive)
  (when-let ((item (todo-explorer--item-at-point)))
    (find-file (todo-explorer-item-file item))
    (goto-char (point-min))
    (forward-line (1- (todo-explorer-item-line item)))
    (when-let ((col (todo-explorer-item-column item)))
      (move-to-column col))))

(defun todo-explorer-visit-item-other-window ()
  "Visit item at point in another window."
  (interactive)
  (when-let ((item (todo-explorer--item-at-point)))
    (find-file-other-window (todo-explorer-item-file item))
    (goto-char (point-min))
    (forward-line (1- (todo-explorer-item-line item)))
    (when-let ((col (todo-explorer-item-column item)))
      (move-to-column col))))


(defun todo-explorer-next-item ()
  "Move to the next item."
  (interactive)
  (forward-line 1))

(defun todo-explorer-prev-item ()
  "Move to the previous item."
  (interactive)
  (forward-line -1))

;;;; Follow Mode

(defun todo-explorer-toggle-follow ()
  "Toggle follow mode (auto-peek source on navigation)."
  (interactive)
  (setq todo-explorer--follow-mode (not todo-explorer--follow-mode))
  (if todo-explorer--follow-mode
      (progn
        (setq todo-explorer--follow-last-item nil)
        (setq todo-explorer--follow-window nil)
        (add-hook 'post-command-hook #'todo-explorer--follow-hook nil t))
    (remove-hook 'post-command-hook #'todo-explorer--follow-hook t)
    (when (and todo-explorer--follow-window
               (window-live-p todo-explorer--follow-window))
      (delete-window todo-explorer--follow-window))
    (setq todo-explorer--follow-window nil)
    (setq todo-explorer--follow-last-item nil))
  (message "Follow mode %s" (if todo-explorer--follow-mode "on" "off")))

(defun todo-explorer--follow-hook ()
  "Peek at current item if it changed since last peek."
  (when todo-explorer--follow-mode
    (let ((item (todo-explorer--item-at-point)))
      (when (and item (not (eq item todo-explorer--follow-last-item)))
        (setq todo-explorer--follow-last-item item)
        (let* ((buf (find-file-noselect (todo-explorer-item-file item)))
               (win (display-buffer buf '(nil (inhibit-same-window . t)))))
          (when win
            (setq todo-explorer--follow-window win)
            (with-selected-window win
              (goto-char (point-min))
              (forward-line (1- (todo-explorer-item-line item)))
              (when-let ((col (todo-explorer-item-column item)))
                (move-to-column col))
              (recenter))))))))

;;;; next-error Integration

(defun todo-explorer--next-error (n &optional reset)
  "Move to the Nth next item and visit its source.
If RESET, go to the first item."
  (when reset
    (goto-char (point-min)))
  (forward-line n)
  (when-let ((item (todo-explorer--item-at-point)))
    (pop-to-buffer (find-file-noselect (todo-explorer-item-file item)))
    (goto-char (point-min))
    (forward-line (1- (todo-explorer-item-line item)))
    (when-let ((col (todo-explorer-item-column item)))
      (move-to-column col))))

;;;; Sort & Filter Commands

(defun todo-explorer-cycle-sort ()
  "Cycle sort mode: file -> keyword -> priority -> context -> age -> file."
  (interactive)
  (setq todo-explorer--sort-mode
        (pcase todo-explorer--sort-mode
          ('file     'keyword)
          ('keyword  'priority)
          ('priority 'context)
          ('context  'age)
          ('age      'file)))
  (todo-explorer--apply-filter-and-sort)
  (message "Sort: %s" todo-explorer--sort-mode))

(defun todo-explorer--set-keyword-filter (keyword)
  "Set keyword filter to KEYWORD and refresh display."
  (setq todo-explorer--active-filter keyword)
  (todo-explorer--apply-filter-and-sort))

(defun todo-explorer-filter-todo ()
  "Show only TODO items."
  (interactive)
  (todo-explorer--set-keyword-filter "TODO"))

(defun todo-explorer-filter-fixme ()
  "Show only FIXME items."
  (interactive)
  (todo-explorer--set-keyword-filter "FIXME"))

(defun todo-explorer-filter-note ()
  "Show only NOTE items."
  (interactive)
  (todo-explorer--set-keyword-filter "NOTE"))

(defun todo-explorer-filter-hack ()
  "Show only HACK items."
  (interactive)
  (todo-explorer--set-keyword-filter "HACK"))

(defun todo-explorer-filter-all ()
  "Remove all filters, show all items."
  (interactive)
  (setq todo-explorer--active-filter nil)
  (setq todo-explorer--priority-filter nil)
  (setq todo-explorer--text-filter nil)
  (setq todo-explorer--file-filter nil)
  (setq todo-explorer--context-filter nil)
  (setq todo-explorer--author-filter nil)
  (todo-explorer--apply-filter-and-sort))

(defun todo-explorer-filter-text (regexp)
  "Filter items by description text matching REGEXP.
With empty input, clear the text filter."
  (interactive (list (read-string
                      (format "Filter description%s (regexp): "
                              (if todo-explorer--text-filter
                                  (format " [%s]" todo-explorer--text-filter)
                                ""))
                      nil nil todo-explorer--text-filter)))
  (setq todo-explorer--text-filter (if (string-empty-p regexp) nil regexp))
  (todo-explorer--apply-filter-and-sort))

(defun todo-explorer-filter-file (regexp)
  "Filter items by file path matching REGEXP.
With empty input, clear the file filter."
  (interactive (list (read-string
                      (format "Filter file path%s (regexp): "
                              (if todo-explorer--file-filter
                                  (format " [%s]" todo-explorer--file-filter)
                                ""))
                      nil nil todo-explorer--file-filter)))
  (setq todo-explorer--file-filter (if (string-empty-p regexp) nil regexp))
  (todo-explorer--apply-filter-and-sort))

(defun todo-explorer-filter-context (regexp)
  "Filter items by context label matching REGEXP.
With empty input, clear the context filter."
  (interactive (list (read-string
                      (format "Filter context%s (regexp): "
                              (if todo-explorer--context-filter
                                  (format " [%s]" todo-explorer--context-filter)
                                ""))
                      nil nil todo-explorer--context-filter)))
  (setq todo-explorer--context-filter (if (string-empty-p regexp) nil regexp))
  (todo-explorer--apply-filter-and-sort))

(defun todo-explorer-filter-priority (level)
  "Show only items with priority <= LEVEL (1=critical, 2=normal, 3=low)."
  (interactive "nPriority level (1-3): ")
  (setq todo-explorer--priority-filter (max 1 (min 3 level)))
  (todo-explorer--apply-filter-and-sort)
  (message "Filter: priority %d and above" todo-explorer--priority-filter))

(defun todo-explorer-filter-priority-1 ()
  "Show only priority 1 items (FIXME/BUG)."
  (interactive)
  (todo-explorer-filter-priority 1))

(defun todo-explorer-filter-priority-2 ()
  "Show priority 1-2 items (FIXME/BUG/TODO/HACK/XXX)."
  (interactive)
  (todo-explorer-filter-priority 2))

(defun todo-explorer-filter-priority-3 ()
  "Show all priority levels (same as clearing priority filter)."
  (interactive)
  (setq todo-explorer--priority-filter nil)
  (todo-explorer--apply-filter-and-sort))

(defun todo-explorer-filter-author (regexp)
  "Filter items by author name matching REGEXP.
With empty input, clear the author filter."
  (interactive (list (read-string
                      (format "Filter author%s (regexp): "
                              (if todo-explorer--author-filter
                                  (format " [%s]" todo-explorer--author-filter)
                                ""))
                      nil nil todo-explorer--author-filter)))
  (setq todo-explorer--author-filter (if (string-empty-p regexp) nil regexp))
  (todo-explorer--apply-filter-and-sort))

;;;; Blame Toggle

(defun todo-explorer-toggle-blame ()
  "Toggle git blame columns (author and age).
Resolves blame data on first activation if not already resolved."
  (interactive)
  (setq todo-explorer--blame-active (not todo-explorer--blame-active))
  (when todo-explorer--blame-active
    ;; Resolve blame if items don't have data yet
    (when (and todo-explorer--items
               (null (todo-explorer-item-author (car todo-explorer--items))))
      (message "Resolving git blame...")
      (todo-explorer--resolve-blame todo-explorer--items
                                    todo-explorer--project-root)))
  (todo-explorer--setup-columns)
  (tabulated-list-init-header)
  (todo-explorer--apply-filter-and-sort)
  (message "Blame %s" (if todo-explorer--blame-active "on" "off")))

;;;; Context Method Toggle

(defun todo-explorer-toggle-context-method ()
  "Cycle context detection method and re-resolve all items.
Cycles: auto -> add-log -> treesit -> auto.
Skips treesit if tree-sitter is not available."
  (interactive)
  (let* ((current (or todo-explorer--context-method
                      todo-explorer-context-method))
         (has-treesit (and (fboundp 'treesit-available-p)
                           (treesit-available-p)))
         (next (pcase current
                 ('auto 'add-log)
                 ('add-log (if has-treesit 'treesit 'auto))
                 ('treesit 'auto))))
    (setq todo-explorer--context-method next)
    (message "Context method: %s (re-resolving...)" next)
    (when todo-explorer--items
      (dolist (item todo-explorer--items)
        (setf (todo-explorer-item-context item) nil))
      (todo-explorer--resolve-contexts todo-explorer--items)
      (todo-explorer--apply-filter-and-sort))
    (message "Context method: %s" next)))

;;;; Ignore Keymap

(defvar todo-explorer-ignore-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'todo-explorer-ignore-item-file)
    (define-key map (kbd "d") #'todo-explorer-ignore-item-directory)
    (define-key map (kbd "l") #'todo-explorer-manage-ignores)
    (define-key map (kbd "c") #'todo-explorer-clear-ignores)
    map)
  "Keymap for ignore commands under the `I' prefix.")

;;;; Filter Keymap

(defvar todo-explorer-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'todo-explorer-filter-todo)
    (define-key map (kbd "f") #'todo-explorer-filter-fixme)
    (define-key map (kbd "n") #'todo-explorer-filter-note)
    (define-key map (kbd "h") #'todo-explorer-filter-hack)
    (define-key map (kbd "a") #'todo-explorer-filter-all)
    (define-key map (kbd "p") #'todo-explorer-filter-file)
    (define-key map (kbd "c") #'todo-explorer-filter-context)
    (define-key map (kbd "1") #'todo-explorer-filter-priority-1)
    (define-key map (kbd "2") #'todo-explorer-filter-priority-2)
    (define-key map (kbd "3") #'todo-explorer-filter-priority-3)
    (define-key map (kbd "w") #'todo-explorer-filter-author)
    map)
  "Keymap for filter commands under the `f' prefix.")

;;;; Column Setup

(defvar-local todo-explorer--last-window-width nil
  "Last known window width, used to detect resize.")

(defun todo-explorer--setup-columns ()
  "Set `tabulated-list-format' based on window width and blame state.
Column widths scale proportionally to the window body width."
  (let* ((width (max 80 (window-body-width)))
         (type-w (max 12 (truncate (* width 0.01))))
         (loc-w  (max 6  (truncate (* width 0.08))))
         (desc-w (max 20 (truncate (* width 0.45))))
         (ctx-w  (max 10 (truncate (* width 0.15))))
         (age-w  (max 4  (truncate (* width 0.04)))))
    (setq todo-explorer--last-window-width width)
    (setq tabulated-list-format
          (if todo-explorer--blame-active
              (vector (list "Type" type-w t)
                      (list "Location" loc-w t)
                      (list "Description" desc-w nil)
                      (list "Context" ctx-w t)
                      (list "Age" age-w t)
                      (list "Author" 0 t))
            (vector (list "Type" type-w t)
                    (list "Location" loc-w t)
                    (list "Description" desc-w nil)
                    (list "Context" 0 t))))))

(defun todo-explorer--on-window-resize ()
  "Recalculate column widths when window width changes."
  (when (derived-mode-p 'todo-explorer-mode)
    (let ((new-width (window-body-width)))
      (unless (eql new-width todo-explorer--last-window-width)
        (todo-explorer--setup-columns)
        (tabulated-list-init-header)))))

;;;; Mode Definition

(defvar todo-explorer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'todo-explorer-visit-item)
    (define-key map (kbd "o")   #'todo-explorer-visit-item-other-window)
    (define-key map (kbd "j")   #'todo-explorer-next-item)
    (define-key map (kbd "k")   #'todo-explorer-prev-item)
    (define-key map (kbd "n")   #'todo-explorer-next-item)
    (define-key map (kbd "p")   #'todo-explorer-prev-item)
    (define-key map (kbd "s")   #'todo-explorer-cycle-sort)
    (define-key map (kbd "f")   todo-explorer-filter-map)
    (define-key map (kbd "F")   #'todo-explorer-toggle-follow)
    (define-key map (kbd "B")   #'todo-explorer-toggle-blame)
    (define-key map (kbd "C")   #'todo-explorer-toggle-context-method)
    (define-key map (kbd "/")   #'todo-explorer-filter-text)
    (define-key map (kbd "g")   #'todo-explorer-refresh)
    (define-key map (kbd "q")   #'todo-explorer-quit)
    (define-key map (kbd "TAB")       #'todo-explorer-toggle-context)
    (define-key map (kbd "<backtab>") #'todo-explorer-toggle-context-all)
    (define-key map (kbd "?")         #'todo-explorer-help)
    (define-key map (kbd "i")   #'todo-explorer-ignore-item)
    (define-key map (kbd "I")   todo-explorer-ignore-map)
    map)
  "Keymap for `todo-explorer-mode'.")

(define-derived-mode todo-explorer-mode tabulated-list-mode "Todo-Explorer"
  "Major mode for browsing project TODO/FIXME/NOTE items.

\\{todo-explorer-mode-map}"
  (setq todo-explorer--blame-active todo-explorer-show-blame)
  (todo-explorer--setup-columns)
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (setq-local next-error-function #'todo-explorer--next-error)
  (tabulated-list-init-header)
  (add-hook 'window-configuration-change-hook
            #'todo-explorer--on-window-resize nil t)
  (hl-line-mode 1)
  (let ((dt (make-display-table)))
    (set-display-table-slot dt 'truncation (make-glyph-code ?…))
    (setq buffer-display-table dt)))

;;;; Buffer Display

(defvar-local todo-explorer--saved-window-config nil
  "Window configuration saved before todo-explorer took over the frame.")

(defun todo-explorer--display-buffer (buf)
  "Display BUF, saving window configuration for later restore.
Uses `todo-explorer-display-action' when set, otherwise full-frame."
  (let ((config (current-window-configuration)))
    (if todo-explorer-display-action
        (pop-to-buffer buf todo-explorer-display-action)
      (pop-to-buffer buf '((display-buffer-same-window)))
      (delete-other-windows))
    (with-current-buffer buf
      (setq todo-explorer--saved-window-config config))))

(defun todo-explorer-quit ()
  "Quit todo-explorer and restore the previous window configuration."
  (interactive)
  (let ((config todo-explorer--saved-window-config))
    (kill-buffer (current-buffer))
    (when config
      (set-window-configuration config))))

;;;; Interactive Commands

;;;###autoload
(defun todo-explorer ()
  "Scan the current project for TODO/FIXME/NOTE keywords.
Results are displayed in a dedicated interactive buffer."
  (interactive)
  (let ((root (funcall todo-explorer-project-root-function)))
    (unless root
      (user-error "No project found; see `project.el' configuration"))
    (let* ((project-name (file-name-nondirectory
                          (directory-file-name root)))
           (buf-name (format "*todo-explorer: %s*" project-name))
           (buf (get-buffer-create buf-name)))
      (with-current-buffer buf
        (unless (derived-mode-p 'todo-explorer-mode)
          (todo-explorer-mode))
        (setq todo-explorer--project-root root)
        (setq todo-explorer--scan-target root)
        (todo-explorer--ensure-ignore-list)
        (setq next-error-last-buffer buf)
        (todo-explorer--scan root buf))
      (todo-explorer--display-buffer buf)
      (todo-explorer--maybe-show-dispatch))))

;;;###autoload
(defun todo-explorer-current-file ()
  "Scan the current file for TODO/FIXME/NOTE keywords."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let* ((file buffer-file-name)
         (root (or (funcall todo-explorer-project-root-function)
                   (file-name-directory file)))
         (buf-name (format "*todo-explorer: %s*"
                           (file-name-nondirectory file)))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'todo-explorer-mode)
        (todo-explorer-mode))
      (setq todo-explorer--project-root root)
      (setq todo-explorer--scan-target file)
      (todo-explorer--ensure-ignore-list)
      (setq next-error-last-buffer buf)
      (todo-explorer--scan file buf))
    (todo-explorer--display-buffer buf)
    (todo-explorer--maybe-show-dispatch)))

(defun todo-explorer-refresh ()
  "Re-scan and refresh the current todo-explorer buffer."
  (interactive)
  (unless (derived-mode-p 'todo-explorer-mode)
    (user-error "Not in a todo-explorer buffer"))
  (when todo-explorer--project-root
    (remhash (expand-file-name todo-explorer--project-root)
             todo-explorer--ignore-data)
    (todo-explorer--ensure-ignore-list))
  (todo-explorer--scan todo-explorer--scan-target (current-buffer)))

;;;; Ignore Commands

(defun todo-explorer--require-project-root ()
  "Return the project root or signal a `user-error'."
  (or todo-explorer--project-root
      (user-error "No project root set")))

(defun todo-explorer-ignore-item ()
  "Add the item at point to the persistent ignore list."
  (interactive)
  (let ((root (todo-explorer--require-project-root))
        (item (todo-explorer--item-at-point)))
    (unless item
      (user-error "No item at point"))
    (todo-explorer--ensure-ignore-list)
    (let* ((rel-file (file-relative-name (todo-explorer-item-file item) root))
           (keyword (todo-explorer-item-keyword item))
           (text (todo-explorer-item-text item))
           (existing (plist-get todo-explorer--ignore-list :items))
           (already (cl-some (lambda (e)
                               (and (string= (todo-explorer-ignore-entry-file e) rel-file)
                                    (string= (todo-explorer-ignore-entry-keyword e) keyword)
                                    (string= (todo-explorer-ignore-entry-text e) text)))
                             existing)))
      (if already
          (message "Already ignored")
        (let ((entry (todo-explorer-ignore-entry-create
                      :file rel-file
                      :keyword keyword
                      :text text
                      :line (todo-explorer-item-line item))))
          (plist-put todo-explorer--ignore-list :items
                     (append existing (list entry)))
          (todo-explorer--save-ignore-list root todo-explorer--ignore-list)
          (todo-explorer--apply-filter-and-sort)
          (message "Ignored: %s %s" keyword text))))))

(defun todo-explorer-ignore-item-file ()
  "Add the file of the item at point to the persistent ignore list."
  (interactive)
  (let ((root (todo-explorer--require-project-root))
        (item (todo-explorer--item-at-point)))
    (unless item
      (user-error "No item at point"))
    (let ((rel-file (file-relative-name (todo-explorer-item-file item) root)))
      (when (y-or-n-p (format "Ignore all items in %s? " rel-file))
        (todo-explorer--ensure-ignore-list)
        (let ((existing (plist-get todo-explorer--ignore-list :files)))
          (if (member rel-file existing)
              (message "File already ignored: %s" rel-file)
            (plist-put todo-explorer--ignore-list :files
                       (append existing (list rel-file)))
            (todo-explorer--save-ignore-list root todo-explorer--ignore-list)
            (todo-explorer--apply-filter-and-sort)
            (message "Ignored file: %s" rel-file)))))))

(defun todo-explorer-ignore-item-directory ()
  "Add a directory to the persistent ignore list.
Prompts with completion from directories containing scanned items."
  (interactive)
  (let ((root (todo-explorer--require-project-root)))
    (todo-explorer--ensure-ignore-list)
    (let* ((leaf-dirs (delete-dups
                       (delq nil
                             (mapcar (lambda (item)
                                       (file-name-directory
                                        (file-relative-name
                                         (todo-explorer-item-file item) root)))
                                     todo-explorer--items))))
           (all-dirs (delete-dups
                      (cl-mapcan
                       (lambda (d)
                         (let ((parts nil) (acc ""))
                           (dolist (seg (split-string d "/" t))
                             (setq acc (concat acc seg "/"))
                             (push acc parts))
                           (nreverse parts)))
                       leaf-dirs)))
           (dir (completing-read "Ignore directory: " all-dirs nil nil)))
      (when (string-empty-p dir)
        (user-error "No directory specified"))
      (unless (string-suffix-p "/" dir)
        (setq dir (concat dir "/")))
      (let ((existing (plist-get todo-explorer--ignore-list :directories)))
        (if (member dir existing)
            (message "Directory already ignored: %s" dir)
          (plist-put todo-explorer--ignore-list :directories
                     (append existing (list dir)))
          (todo-explorer--save-ignore-list root todo-explorer--ignore-list)
          (todo-explorer--apply-filter-and-sort)
          (message "Ignored directory: %s" dir))))))

(defun todo-explorer-clear-ignores ()
  "Clear all ignore entries for the current project."
  (interactive)
  (let ((root (todo-explorer--require-project-root)))
    (when (yes-or-no-p "Clear all ignore entries for this project? ")
      (let ((empty (list :items nil :files nil :directories nil)))
        (setq todo-explorer--ignore-list empty)
        (todo-explorer--save-ignore-list root empty)
        (todo-explorer--apply-filter-and-sort)
        (message "All ignores cleared")))))

;;;; Ignore Management Buffer

(defvar todo-explorer-ignore-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "d") #'todo-explorer-ignore-mark-delete)
    (define-key map (kbd "u") #'todo-explorer-ignore-unmark)
    (define-key map (kbd "x") #'todo-explorer-ignore-execute)
    (define-key map (kbd "RET") #'todo-explorer-ignore-visit)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `todo-explorer-ignore-mode'.")

(define-derived-mode todo-explorer-ignore-mode tabulated-list-mode
  "Todo-Ignore"
  "Major mode for managing todo-explorer ignore entries.

\\{todo-explorer-ignore-mode-map}"
  (setq tabulated-list-format
        (vector '(" " 1 t)
                '("Type" 6 t)
                '("Target" 40 t)
                '("Details" 0 nil)))
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defvar-local todo-explorer-ignore--project-root nil
  "Project root for the ignore management buffer.")

(defvar-local todo-explorer-ignore--source-buffer nil
  "The todo-explorer buffer that spawned the management buffer.")

(defun todo-explorer-manage-ignores ()
  "Open a management buffer to view and remove ignore entries."
  (interactive)
  (let ((root (todo-explorer--require-project-root))
        (source-buf (current-buffer)))
    (todo-explorer--ensure-ignore-list)
    (let* ((project-name (file-name-nondirectory
                           (directory-file-name root)))
           (buf-name (format "*todo-explorer-ignores: %s*" project-name))
           (buf (get-buffer-create buf-name)))
      (with-current-buffer buf
        (todo-explorer-ignore-mode)
        (setq todo-explorer-ignore--project-root root)
        (setq todo-explorer-ignore--source-buffer source-buf)
        (todo-explorer--ignore-refresh-entries root))
      (pop-to-buffer buf))))

(defun todo-explorer--ignore-refresh-entries (root)
  "Populate the ignore management buffer with entries for ROOT."
  (let ((data (or (gethash (expand-file-name root) todo-explorer--ignore-data)
                  (todo-explorer--load-ignore-list root)))
        (entries nil))
    (dolist (entry (plist-get data :items))
      (let* ((file (todo-explorer-ignore-entry-file entry))
             (kw (todo-explorer-ignore-entry-keyword entry))
             (text (todo-explorer-ignore-entry-text entry)))
        (push (list (cons 'item entry)
                    (vector " " "Item" file
                            (format "%s: %s" kw text)))
              entries)))
    (dolist (file (plist-get data :files))
      (push (list (cons 'file file)
                  (vector " " "File" file ""))
            entries))
    (dolist (dir (plist-get data :directories))
      (push (list (cons 'dir dir)
                  (vector " " "Dir" dir ""))
            entries))
    (setq tabulated-list-entries (nreverse entries))
    (tabulated-list-print t)))

(defun todo-explorer-ignore-mark-delete ()
  "Mark the entry at point for deletion."
  (interactive)
  (let ((entry (tabulated-list-get-entry)))
    (when entry
      (tabulated-list-put-tag "D" t))))

(defun todo-explorer-ignore-unmark ()
  "Unmark the entry at point."
  (interactive)
  (let ((entry (tabulated-list-get-entry)))
    (when entry
      (tabulated-list-put-tag " " t))))

(defun todo-explorer-ignore-execute ()
  "Remove all entries marked with `D' and save."
  (interactive)
  (let ((root todo-explorer-ignore--project-root)
        (to-remove nil))
    (unless root
      (user-error "No project root"))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (eq (char-after) ?D)
          (let ((id (tabulated-list-get-id)))
            (when id (push id to-remove))))
        (forward-line 1)))
    (when (and to-remove
               (y-or-n-p (format "Remove %d ignore entries? "
                                 (length to-remove))))
      (let ((data (or (gethash (expand-file-name root)
                               todo-explorer--ignore-data)
                      (todo-explorer--load-ignore-list root))))
        (dolist (entry to-remove)
          (pcase (car entry)
            ('item
             (let ((e (cdr entry)))
               (plist-put data :items
                          (cl-remove-if
                           (lambda (x)
                             (and (string= (todo-explorer-ignore-entry-file x)
                                           (todo-explorer-ignore-entry-file e))
                                  (string= (todo-explorer-ignore-entry-keyword x)
                                           (todo-explorer-ignore-entry-keyword e))
                                  (string= (todo-explorer-ignore-entry-text x)
                                           (todo-explorer-ignore-entry-text e))))
                           (plist-get data :items)))))
            ('file
             (plist-put data :files
                        (delete (cdr entry) (plist-get data :files))))
            ('dir
             (plist-put data :directories
                        (delete (cdr entry) (plist-get data :directories))))))
        (todo-explorer--save-ignore-list root data)
        (todo-explorer--ignore-refresh-entries root)
        ;; Refresh source buffer if alive
        (when (and todo-explorer-ignore--source-buffer
                   (buffer-live-p todo-explorer-ignore--source-buffer))
          (with-current-buffer todo-explorer-ignore--source-buffer
            (setq todo-explorer--ignore-list data)
            (todo-explorer--apply-filter-and-sort)))
        (message "Removed %d ignore entries" (length to-remove))))))

(defun todo-explorer-ignore-visit ()
  "Visit the file of the ignore entry at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No entry at point"))
    (let* ((root todo-explorer-ignore--project-root)
           (file (pcase (car id)
                   ('item (todo-explorer-ignore-entry-file (cdr id)))
                   ('file (cdr id))
                   ('dir (cdr id)))))
      (when file
        (let ((abs-file (expand-file-name file root)))
          (if (file-exists-p abs-file)
              (find-file-other-window abs-file)
            (message "File does not exist: %s" abs-file)))))))

;;;; Transient Dispatch

(with-eval-after-load 'transient
  (eval
   '(transient-define-prefix todo-explorer-dispatch ()
      "Command dispatch for todo-explorer."
      :transient-suffix 'transient--do-stay
      [["Navigate"
        ("RET" "Visit item" todo-explorer-visit-item :transient nil)
        ("o" "Other window" todo-explorer-visit-item-other-window :transient nil)
        ("j" "Next item" todo-explorer-next-item)
        ("k" "Previous item" todo-explorer-prev-item)
        ("n" "Next item" todo-explorer-next-item)
        ("p" "Previous item" todo-explorer-prev-item)]
       ["Sort & Filter"
        ("s" "Cycle sort" todo-explorer-cycle-sort)
        ("/" "Search text" todo-explorer-filter-text)
        ("f t" "TODO only" todo-explorer-filter-todo)
        ("f f" "FIXME only" todo-explorer-filter-fixme)
        ("f n" "NOTE only" todo-explorer-filter-note)
        ("f h" "HACK only" todo-explorer-filter-hack)
        ("f p" "Filter path" todo-explorer-filter-file)
        ("f c" "Filter context" todo-explorer-filter-context)
        ("f 1" "Priority 1" todo-explorer-filter-priority-1)
        ("f 2" "Priority 1-2" todo-explorer-filter-priority-2)
        ("f 3" "All priorities" todo-explorer-filter-priority-3)
        ("f w" "Filter author" todo-explorer-filter-author)
        ("f a" "Show all" todo-explorer-filter-all)]
       ["Actions"
        ("TAB" "Expand/collapse" todo-explorer-toggle-context)
        ("<backtab>" "Expand/collapse all" todo-explorer-toggle-context-all)
        ("F" "Follow mode" todo-explorer-toggle-follow)
        ("B" "Toggle blame" todo-explorer-toggle-blame)
        ("C" "Context method" todo-explorer-toggle-context-method)
        ("g" "Refresh" todo-explorer-refresh)
        ("?" "Close menu" transient-quit-one)
        ("q" "Quit" todo-explorer-quit :transient nil)]
       ["Ignore"
        ("i" "Ignore item" todo-explorer-ignore-item)
        ("I f" "Ignore file" todo-explorer-ignore-item-file)
        ("I d" "Ignore directory" todo-explorer-ignore-item-directory)
        ("I l" "Manage ignores" todo-explorer-manage-ignores)
        ("I c" "Clear ignores" todo-explorer-clear-ignores)]])
   t))

(defun todo-explorer-help ()
  "Show todo-explorer command help.
Uses `transient' dispatch when available, otherwise a help buffer."
  (interactive)
  (if (fboundp 'todo-explorer-dispatch)
      (todo-explorer-dispatch)
    (with-help-window "*todo-explorer help*"
      (princ "todo-explorer — Keybinding Reference\n")
      (princ (make-string 37 ?─))
      (princ "\n\n")
      (princ "Navigate\n")
      (princ "  RET       Visit source location\n")
      (princ "  o         Visit in other window\n")
      (princ "  j / n     Next item\n")
      (princ "  k / p     Previous item\n")
      (princ "\nSort & Filter\n")
      (princ "  s         Cycle sort (file → keyword → priority → context → age)\n")
      (princ "  /         Filter by description text (regexp)\n")
      (princ "  f t       TODO only\n")
      (princ "  f f       FIXME only\n")
      (princ "  f n       NOTE only\n")
      (princ "  f h       HACK only\n")
      (princ "  f p       Filter by file path (regexp)\n")
      (princ "  f c       Filter by context (regexp)\n")
      (princ "  f 1       Priority 1 only (FIXME/BUG)\n")
      (princ "  f 2       Priority 1-2 (FIXME/BUG/TODO/HACK/XXX)\n")
      (princ "  f 3       All priorities\n")
      (princ "  f w       Filter by author (regexp, requires blame)\n")
      (princ "  f a       Clear all filters\n")
      (princ "\nActions\n")
      (princ "  TAB       Expand/collapse context lines\n")
      (princ "  S-TAB     Expand/collapse all\n")
      (princ "  F         Toggle follow mode (auto-peek source)\n")
      (princ "  B         Toggle git blame (author + age columns)\n")
      (princ "  C         Cycle context method (auto → add-log → treesit)\n")
      (princ "  g         Refresh (rescan)\n")
      (princ "  q         Quit and restore window layout\n")
      (princ "\nIgnore\n")
      (princ "  i         Ignore item at point\n")
      (princ "  I f       Ignore file of item at point\n")
      (princ "  I d       Ignore directory (with completion)\n")
      (princ "  I l       Manage ignore list\n")
      (princ "  I c       Clear all ignores\n"))))

(defun todo-explorer--maybe-show-dispatch ()
  "Show the dispatch menu if configured and transient is available."
  (when (and todo-explorer-show-dispatch
             (fboundp 'todo-explorer-dispatch))
    (run-at-time 0 nil #'todo-explorer-dispatch)))

;;;; imenu Integration

(defvar-local todo-explorer--orig-imenu-function nil
  "Saved original `imenu-create-index-function' before imenu-mode activation.")

(defun todo-explorer--imenu-todo-entries ()
  "Scan current buffer for TODO keywords and return imenu-style entries.
Returns a list of (LABEL . MARKER) cons cells.
Uses word boundaries (not symbol boundaries) so keywords match
regardless of the buffer's syntax table (e.g. `:' is a symbol
constituent in `emacs-lisp-mode')."
  (let ((regexp (concat "\\<"
                        (regexp-opt (todo-explorer--effective-keywords) t)
                        "\\>"))
        entries)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (while (re-search-forward regexp nil t)
          (let* ((keyword (match-string-no-properties 1))
                 (pos (match-beginning 0))
                 (text (string-trim
                        (buffer-substring-no-properties
                         (match-end 0) (line-end-position)))))
            ;; Strip leading colon/dash
            (when (string-match "\\`[: -]+" text)
              (setq text (string-trim (substring text (match-end 0)))))
            (let ((label (if (string-empty-p text)
                             (format "%s (line %d)" keyword
                                     (line-number-at-pos pos))
                           (format "%s: %s" keyword
                                   (truncate-string-to-width text 60)))))
              (push (cons label (copy-marker pos)) entries))))))
    (nreverse entries)))

(defun todo-explorer--imenu-create-index ()
  "Create imenu index with TODO/FIXME/NOTE entries appended.
Calls the original `imenu-create-index-function' first, then appends
TODO entries under a \"TODO\" submenu."
  (let ((original (when (functionp todo-explorer--orig-imenu-function)
                    (funcall todo-explorer--orig-imenu-function)))
        (todos (todo-explorer--imenu-todo-entries)))
    (if todos
        (append original (list (cons "TODO" todos)))
      original)))

;;;###autoload
(define-minor-mode todo-explorer-imenu-mode
  "Minor mode to add TODO/FIXME/NOTE entries to `imenu' in source buffers.
When enabled, `imenu' (and packages like `consult-imenu') will include
a \"TODO\" submenu listing all TODO-style keywords in the buffer."
  :lighter nil
  (if todo-explorer-imenu-mode
      (progn
        (setq-local todo-explorer--orig-imenu-function
                    imenu-create-index-function)
        (setq-local imenu-create-index-function
                    #'todo-explorer--imenu-create-index))
    (when todo-explorer--orig-imenu-function
      (setq-local imenu-create-index-function
                  todo-explorer--orig-imenu-function)
      (setq-local todo-explorer--orig-imenu-function nil))))

;;;; Evil Compatibility

(declare-function evil-set-initial-state "evil-core" (mode state))

(with-eval-after-load 'evil
  (evil-set-initial-state 'todo-explorer-mode 'emacs))

(provide 'todo-explorer)
;;; todo-explorer.el ends here
