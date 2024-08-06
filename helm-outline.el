;;; helm-outline.el --- helm for searching document outline   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  David Shilvock

;; Author: David Shilvock <dshlvock@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A couple of simple helms for searching the buffer outline.

;;; Code:

(require 'helm)
(require 'outline-toc)


(defun helm-outline-candidates (&optional buffer)
  (let ((cand (outline-toc-scan-buffer (or buffer helm-current-buffer))))
    ;; (pos level label)  ==>  (label . pos)
    (mapcar (lambda (item)
              (cons (concat (make-string (* 2 (1- (nth 1 item))) ?\s)
                            (nth 2 item))
                    (nth 0 item)))
            cand)))
    
(defvar helm-source-outline '((name . "Outline")
                              (candidates . helm-outline-candidates)
                              (filtered-candidate-transformer helm-fuzzy-highlight-matches)
                              (nomark)
                              (action . (lambda (cand) (goto-char cand)))))

;;;###autoload
(defun helm-outline ()
  "helm for searching the outline of the current buffer."
  (interactive)
  (helm :sources '(helm-source-outline)
        :candidate-number-limit 250
        :truncate-line t
        :default ""
        :preselect ""
        :buffer " *helm outline*"))


(eval-when-compile (require 'helm-imenu))

;;;###autoload
(defun helm-outline-and-imenu ()
  "helm for searching the outline and imenu of the current buffer."
  (interactive)
  (require 'helm-imenu)
  (unless helm-source-imenu
    (setq helm-source-imenu
          (helm-make-source "Imenu" 'helm-imenu-source
            :fuzzy-match helm-imenu-fuzzy-match)))
  (let ((imenu-auto-rescan t))
    (helm :sources '(helm-source-outline helm-source-imenu)
          :candidate-number-limit 250
          :truncate-line t
          :default ""
          :preselect ""
          :buffer " *helm index*")))


(provide 'helm-outline)
;;; helm-outline.el ends here
