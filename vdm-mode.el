;;; vdm-mode.el --- Major mode for the Vienna Development Method -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Peter W. V. Tran-Jørgensen
;; Author: Peter W. V. Tran-Jørgensen <peter.w.v.jorgensen@gmail.com>
;; Maintainer: Peter W. V. Tran-Jørgensen <peter.w.v.jorgensen@gmail.com>
;; URL: https://github.com/peterwvj/vdm-mode
;; Created: 29th August 2018
;; Version: 0.0.3
;; Keywords: languages
;; Package-Requires: ((emacs "25"))

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; vdm-mode currently supports the following features:

;; - Syntax highlighting and editing
;; - Replacement of ASCII syntax (e.g. lambda) with more aesthetically
;;   looking symbols (e.g. λ) using prettify-symbols-mode
;; - On the fly syntax checking using Flycheck
;;   - Integration with VDMJ and Overture
;; - VDM YASnippets


;;; Code:

(require 'vdm-mode-util)

(require 'vdm-snippets nil 'noerror)
(require 'flycheck-vdm nil 'noerror)

;; Inconvenient to treat ` as a pair in vdm-mode
(when (bound-and-true-p smartparens-mode)
  (sp-local-pair #'vdm-mode "`" nil :actions nil))

;;;###autoload
(let* ((vdm-files '(".vdmsl" ".vsl" ".vdmpp" ".vpp" ".vdmrt" ".vrt"))
       (vdm-regexp (concat (regexp-opt vdm-files t) "\\'")))
  (add-to-list 'auto-mode-alist (cons vdm-regexp 'vdm-mode)))

(defconst vdm-mode-shared-keywords
  '("RESULT" "abs" "all" "always" "and" "atomic" "be"
    "by" "card" "cases" "comp" "compose" "conc" "dcl"
    "def" "dinter" "div" "do" "dom" "dunion" "elems" "else"
    "elseif" "end" "eq" "error" "errs" "exists" "exists1"
    "exit" "ext" "floor" "for" "forall" "from"
    "functions" "hd" "if" "in" "inds" "inmap" "inter"
    "inv" "inverse" "iota" "is" "lambda" "len" "let" "map"
    "measure" "merge" "mod" "mu" "munion"
    "not" "of" "operations" "or" "ord" "others" "post" "power"
    "pre" "psubset" "pure" "rd" "rem" "return"
    "reverse" "rng" "seq" "seq1" "set" "set1" "skip"
    "specified" "st" "subset" "then" "tixe" "tl" "to"
    "traces" "trap" "types" "undefined" "union"
    "values" "while" "with" "wr" "yet")
  "VDM keywords shared by VDM-SL, VDM-PP and VDM-RT.")

(defconst vdm-mode-vdmsl-keywords
  (append vdm-mode-shared-keywords
          '("as" "definitions" "dlmodule" "exports" "imports"
            "init" "module" "renamed" "state" "struct" "uselib"))
  "All VDM-SL keywords.")

(defconst vdm-mode-vdmpp-keywords
  (append vdm-mode-shared-keywords
          '("#act" "#active" "#fin" "#req" "#waiting" "async"
            "class" "instance" "isofbaseclass" "isofclass"
            "mutex" "new" "per" "periodic" "private"
            "protected" "public" "responsibility"
            "samebaseclass" "sameclass" "self" "sporadic"
            "start" "startlist" "static" "stop" "stoplist"
            "subclass" "sync" "thread" "threadid" "variables"))
  "All VDM-PP keywords.")


;; The 'not' keyword is used for negation but also appear in the 'is
;; not yet specified' construct. Therefore it will not be prettyfied
;; using ("not" . ?¬).
(defconst vdm-mode-prettify-symbols
  '(("nat" . ?ℕ) ("int" . ?ℤ) ("rat" . ?ℚ) ("real" . ?ℝ) ("bool" . ?𝔹)
   ("&" . ?⋅) ("and" . ?∧) ("or" . ?∨) ("=>" . ?⇒) ("<=>" . ?≡)
   ("==" . ?≜) ("in set" . ?∈) ("not in set" . ?∉) ("<>" . ?≠) ("<=" . ?≤)
   (">=" . ?≥) ("|->" . ?↦) ("div" . ?÷) ("subset" . ?⊆) ("undefined" . ?⊢)
   ("->" . ?⭢) ("inter" . ?∩) ("union" . ?∪) ("*" . ?×) ("exists" . ?∃)
   ("forall" . ?∀)("lambda" . ?λ))
  "VDM symbol prettifications.")

(defconst vdm-mode-vdmrt-keywords
  (append vdm-mode-vdmpp-keywords '("system" "cycles" "duration" "time"))
  "All VDM-RT keywords.")

(defconst vdm-mode-constant-regex
  (concat
   ;; Quote literals, e.g. <Green>
   "<[^\s]+?>\\|"
   ;; Character literals, e.g. 'x' or '\n'
   "'..?'\\|"
   (regexp-opt '("true" "false" "nil") 'words))
  "Regex for VDM constants.")

(defconst vdm-mode-type-regex
  (regexp-opt '("bool" "char" "int" "nat" "nat1" "rat" "real" "token") 'words)
  "Regex for VDM types.")

(defconst vdm-mode-negation-char-regex
  "[`:;\\.\\(\\){}]"
  "Regex for VDM negation chars.")

(defun vdm-mode-get-keywords ()
  "Get VDM keywords based on dialect."
    (cond ((vdm-mode-util-is-sl) vdm-mode-vdmsl-keywords)
          ((vdm-mode-util-is-pp) vdm-mode-vdmpp-keywords)
          ((vdm-mode-util-is-rt) vdm-mode-vdmrt-keywords)))

(defun vdm-mode-get-keyword-regex ()
  "Get regex for VDM keywords based on dialect."
  (concat
   ;; Record, tuple and token constructors
   "mk_\\(?:token\\)?\\|"
   (regexp-opt (vdm-mode-get-keywords) 'words)))

(defun vdm-mode-create-project ()
  "Read directory name and create a VDM project file in that directory."
  (interactive)
  (let* ((dir (read-directory-name "Root of VDM project: "))
         (project-file (expand-file-name vdm-mode-util-project-file dir)))
    (if (file-exists-p project-file)
        (message (concat project-file " already exists."))
      (progn
        (write-region "" nil project-file)
        (message (concat "Created VDM project in " dir))))))

(defun vdm-mode-setup ()
  "Enable YASnippet and Flycheck when 'vdm-mode' is loaded."
  (add-hook 'vdm-mode-hook
	          (lambda ()
	            (progn
                (when (boundp 'yas-minor-mode)
                  (yas-minor-mode 1))
		            (when (boundp 'flycheck-mode)
                  (flycheck-mode 1))))))

(defconst vdm-mode-syntax-table
  (let ((table (make-syntax-table)))
    
    ;; " is a string delimiter
    (modify-syntax-entry ?\" "\"" table)

    ;; ' is a character character delimiter
    (modify-syntax-entry ?' "/" table)

    ;; / is punctuation and the first and last characters of a comment (comment sequence a)
    (modify-syntax-entry ?/ ". 14" table)

    ;; * is punctuation and the second and second last characters of a comment (comment sequence a)
    (modify-syntax-entry ?* ". 23" table)

    ;; - is punctuation but -- is a comment starter (comment sequence b)
    (modify-syntax-entry ?- ". 12 b" table)

    ;; \n is a comment ender (comment sequence b)
    (modify-syntax-entry ?\n "> b" table)

    table))

;;;###autoload
(define-derived-mode vdm-mode prog-mode "VDM mode"
  :syntax-table vdm-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults) `((
                                                    ( ,vdm-mode-constant-regex . font-lock-constant-face)
                                                    ( ,(vdm-mode-get-keyword-regex) . font-lock-keyword-face)
                                                    ( ,vdm-mode-type-regex . font-lock-type-face)
                                                    ( ,vdm-mode-negation-char-regex . font-lock-negation-char-face)
                                                    )))
  ;; For writing comments
  (set (make-local-variable 'comment-start) "--")

  (when (version<= "24.4" emacs-version)
    (progn
      (set (make-local-variable 'prettify-symbols-alist) vdm-mode-prettify-symbols)
      (prettify-symbols-mode))))

(provide 'vdm-mode)

;;; vdm-mode.el ends here
