;;; vdm-snippets.el --- YASnippets for VDM mode -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Peter W. V. Tran-Jørgensen
;; Author: Peter W. V. Tran-Jørgensen <peter.w.v.jorgensen@gmail.com>
;; Maintainer: Peter W. V. Tran-Jørgensen <peter.w.v.jorgensen@gmail.com>
;; URL: https://github.com/peterwvj/vdm-mode
;; Created: 29th August 2018
;; Version: 0.0.4
;; Keywords: languages
;; Package-Requires: ((emacs "24") (yasnippet "0.13.0"))

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
;; Loads a collection of VDM YASnippets.

;;; Code:


(require 'yasnippet)

(defvar vdm-snippets-root
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Root directory of VDM snippets.")

;;;###autoload
(defun vdm-snippets-initialize ()
  "Initialize VDM snippets such that YASnippet can see them."
  (let ((dir (expand-file-name "snippets" vdm-snippets-root)))
    (when (boundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs dir t))
    (yas-load-directory dir)))

;;;###autoload
(eval-after-load 'yasnippet
  '(vdm-snippets-initialize))


(provide 'vdm-snippets)
;;; vdm-snippets.el ends here

