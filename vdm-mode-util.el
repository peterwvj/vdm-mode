;;; vdm-mode-util.el --- Utility functions for vdm-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Peter W. V. Tran-Jørgensen
;; Author: Peter W. V. Tran-Jørgensen <peter.w.v.jorgensen@gmail.com>
;; Maintainer: Peter W. V. Tran-Jørgensen <peter.w.v.jorgensen@gmail.com>
;; URL: https://github.com/peterwvj/vdm-mode
;; Created: 29th August 2018
;; Version: 0.0.2
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
;; Contains utility functionality for vdm-mode

;;; Code:

(require 'seq)

(defconst vdm-mode-util-project-file ".vdm-project"
  "A file that marks the root of a VDM project.")

(defun vdm-mode-util-file-name-extension ()
  "Get the extension of the file associated with the current buffer, nil if none."
  (if buffer-file-name
      (file-name-extension buffer-file-name)
    nil))

(defun vdm-mode-util-is-sl ()
  "Return t if the current file is a VDM-SL file, nil otherwise."
  (let ((extension (vdm-mode-util-file-name-extension)))
    (and extension
         (or (string= extension "vdmsl")
             (string= extension "vsl")))))

(defun vdm-mode-util-is-pp ()
  "Return t if the current file is a VDM++ file, nil otherwise."
  (let ((extension (vdm-mode-util-file-name-extension)))
    (and extension
         (or (string= extension "vdmpp")
             (string= extension "vpp")))))

(defun vdm-mode-util-is-rt ()
  "Return t if the current file is a VDM-RT file, nil otherwise."
  (let ((extension (vdm-mode-util-file-name-extension)))
    (and extension
         (or (string= extension "vdmrt")
             (string= extension "vrt")))))

(defun vdm-mode-util-is-pp-or-rt ()
  "Return t if the current file is a VDM++ or VDM-RT file, nil otherwise."
  (or (vdm-mode-util-is-pp)
      (vdm-mode-util-is-rt)))

(defun vdm-mode-util-is-vdm ()
  "Return t if the current file is a VDM file (VDM-SL, VDM++ or VDM-RT), nil otherwise."
  (or (vdm-mode-util-is-sl)
      (vdm-mode-util-is-pp)
      (vdm-mode-util-is-rt)))

(defun vdm-mode-util-find-vdm-files ()
  "Return all VDM files associated with the current project."
  (let ((file-ext (vdm-mode-util-file-name-extension)))
    (if (vdm-mode-util-is-vdm)
        (let ((project-root (locate-dominating-file default-directory vdm-mode-util-project-file)))
          (if project-root
              (let ((vdm-file-regex (concat "\\" file-ext "$")))
                (let ((vdm-files (directory-files-recursively project-root vdm-file-regex)))
                  (seq-filter (lambda (file)
                                (and
                                 (not (string-match-p "/\\.#.+$" file))
                                 (not (string= (buffer-file-name) file))))
                              vdm-files)))
            '()))
      '())))

(provide 'vdm-mode-util)

;;; vdm-mode-util.el ends here
