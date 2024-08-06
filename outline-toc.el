;;; outline-toc.el --- show buffer outline in a separate buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: <dshlvock@gmail.com>
;; Keywords: outlines

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

;; This package contains a function `outline-toc-show' to display the outline headings of
;; the current file in a *TOC* buffer.  The file does not require `outline-mode' or
;; `outline-minor-mode' to be active, just that the values of `outline-regexp' and
;; `outline-level' be valid.
;;
;; It provides commands to navigate the toc buffer and toggle visiblity of the
;; headings. Use `outline-toc-toggle-follow-mode' to enable cursor motion in the TOC buffer
;; to automatically display the source buffer location.
;;
;; The TOC buffer can be set to automatically update when the source buffer is modified
;; with the command `outline-toc-toggle-autosync'.
;;
;; NOTE When the toc buffer is updated, either manually or via automatic sync, it will
;; clobber any show/hide state and display the fully expanded toc tree.

;;; Code:

(require 'outline)

(defvar outline-toc-header-face 'bold
  "Face used for the toc header.")

(defvar outline-toc-level-indent 4
  "Number of spaces to indent for each heading level.")

(defvar outline-toc-label-extract-style 'line
  "Method used to extract the label from an outline heading.
The value `line' will copy the entire line.  Any other value will copy the line
excluding the initial substring that matches `outline-regexp'.")
;;;###autoload(put 'outline-toc-label-extract-style 'safe-local-variable 'symbolp)

(defvar outline-toc-goto-heading-hook nil
  "Hook run after jumping to a heading in the source buffer.")

(defvar-local outline-toc-follow-mode nil
  "Non-nil if follow mode is enabled.
Do not modify this variable. Instead use the function `outline-toc-toggle-follow-mode'")

(defvar-local outline-toc-autosync nil
  "Non-nil if autosync is enabled.
Do not modify this variable. Instead use the function `outline-toc-toggle-autosync'.")


;;; internals

(defvar-local outline-toc-follow-prev-line -1)

;; shared by all toc buffers
(defvar outline-toc-sync-timer nil)

(defvar-local outline-toc-source-buffer nil)
(defvar-local outline-toc-source-modified-tick 0)

(defvar outline-toc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g"         #'outline-toc-refresh)  ; replace revert-buffer
    (define-key map [mouse-2]   #'outline-toc-mouse-goto-heading)
    (define-key map (kbd "RET") #'outline-toc-goto-heading)
    (define-key map "o"         #'outline-toc-goto-heading-other-window)
    (define-key map "\C-o"      #'outline-toc-display-heading)
    (define-key map "n"         #'next-line)
    (define-key map "p"         #'previous-line)
    (define-key map "S"         #'outline-toc-toggle-autosync)
    (define-key map (kbd "TAB") #'outline-toc-toggle-children)
    (define-key map "h"         #'outline-hide-sublevels)
    (define-key map "a"         #'outline-show-all)
    (define-key map "f"         #'outline-forward-same-level)
    (define-key map "b"         #'outline-backward-same-level)
    (define-key map "u"         #'outline-up-heading)
    (define-key map (kbd "C-c C-f") #'outline-toc-toggle-follow-mode)
    map)
  "Keymap for `outline-toc-mode'.")

(defvar outline-toc-mode-name "TOC")

(put 'outline-toc-mode 'mode-class 'special)
(define-derived-mode outline-toc-mode special-mode 'outline-toc-mode-name
  "Major mode for showing a buffer outline table of contents.
\\<outline-toc-mode-map>
\\{outline-toc-mode-map}"
  ;; from `outline' to navigate outline and toggle visibility of subtrees
  (setq-local outline-regexp " +")
  (setq-local outline-level
              (lambda ()
                (1+ (/ (1- (current-indentation)) outline-toc-level-indent))))
  (setq-local line-move-ignore-invisible t)
  (add-to-invisibility-spec '(outline . t)))

(defun outline-toc-update-mode-name ()
  (setq outline-toc-mode-name
        (concat "TOC"
                (if (or outline-toc-follow-mode outline-toc-autosync) "/")
                (if outline-toc-follow-mode "f")
                (if outline-toc-autosync "a")))
  (force-mode-line-update))

;;;###autoload
(defun outline-toc-show ()
  "Show an ouline for the current buffer in a *TOC* buffer.
The outline is extracted using `outline-regexp' and the function specified by the
variable `outline-level'."
  (interactive)
  (let* ((srcbuffer (current-buffer))
         (filename buffer-file-truename)
         (tocbuf (get-buffer-create
                  (format "*TOC:%s*"
                          (and filename (file-name-nondirectory filename))))))
    (with-current-buffer tocbuf
      (outline-toc-mode)
      (setq outline-toc-source-buffer srcbuffer
            outline-toc-source-modified-tick (buffer-chars-modified-tick srcbuffer))
      (outline-toc-show-1 tocbuf))
    (pop-to-buffer tocbuf)))  ;display-buffer
  
(defun outline-toc-show-1 (tocbuf &optional syncpos synccol)
  ;; TODO use get-buffer-window-list to get all windows showing buffer
  (let* ((toc-window (get-buffer-window tocbuf))  
         (toc-window-start (and toc-window (window-start toc-window)))
         target-pt)
    (with-current-buffer tocbuf
      (when (buffer-live-p outline-toc-source-buffer)
        (let* ((inhibit-read-only t)
               (candlist (outline-toc-scan-buffer outline-toc-source-buffer)))
          (erase-buffer)
          (insert (propertize (format "Outline for %s\n"
                                      (buffer-local-value 'buffer-file-truename
                                                          outline-toc-source-buffer))
                              'face outline-toc-header-face))
          (dolist (item candlist)
            (let* ((srcpos (nth 0 item))
                   (level (nth 1 item))
                   (label (concat
                           " "
                           (make-string (* outline-toc-level-indent (1- level)) ?\s)
                           (nth 2 item)
                           ;;(propertize (nth 2 item) 'face (intern (format "outline-%d" level)))
                           "\n")))
              (put-text-property 0 1 'srcpos srcpos label)
              ;; try to keep cursor on same outline entry
              (if (and syncpos (<= srcpos syncpos))
                  (setq target-pt (line-beginning-position)))
              (insert label))))
        (goto-char (point-min))
        (when target-pt          
          ;; restore window scroll pos
          (if toc-window-start  ;; try to preserve the scroll pos before setting point
              (set-window-start toc-window toc-window-start))
          ;; restore buffer pos
          (goto-char target-pt)
          (if synccol
              (let ((line-length (- (line-end-position) (line-beginning-position))))
                (goto-char (+ (point) (min synccol line-length))))))
        (set-buffer-modified-p nil)))))

(defun outline-toc-scan-buffer (buffer)
  "Build a list of (pos level label) for all outline headings in BUFFER.
Heading levels will be normalized to start at 1 and increase for each level used."
  (with-current-buffer buffer
    (let* ((pattern (concat "^\\(?:" outline-regexp "\\)"))
           (levelmap)
           (cand (save-excursion
                   (goto-char (point-min))
                   (cl-loop while (re-search-forward pattern nil t)
                            as line-begin = (match-beginning 0)
                            as label = (save-match-data
                                         (string-trim
                                          (buffer-substring-no-properties
                                           (cond ((eq outline-toc-label-extract-style 'line)
                                                  line-begin)
                                                 (t ;exclude-match
                                                  (match-end 0)))
                                           (line-end-position))))
                            as level = (progn (goto-char line-begin)
                                              (funcall outline-level))
                            do 
                            (unless (memql level levelmap)
                              (setq levelmap (cons level levelmap)))
                            (goto-char (line-end-position))
                            collect (list line-begin level label)))))
      ;; reported outline levels can have huge gaps and go to extreme numbers (1000).
      ;; normalize and adjust the reported outline levels to start at 1 and increase by 1
      ;; for each actual level in the buffer.
      (sort levelmap #'<)
      (setq levelmap (let (res)
                     (dolist (item levelmap res)
                       (setq res (cons (cons item (1+ (length res))) res)))))
      (dolist (c cand)
        (setf (nth 1 c)
              (alist-get (nth 1 c) levelmap nil 'eql)))
      cand)))

(defun outline-toc-find-target-noerror ()
  (get-text-property (line-beginning-position) 'srcpos))

(defun outline-toc-find-target ()
  (let ((pos (outline-toc-find-target-noerror)))
    (unless pos
      (error "No target on this line"))
    (unless (buffer-live-p outline-toc-source-buffer)
      (error "Source buffer was killed"))
    pos))

;;; refresh and auto sync

(defun outline-toc-refresh ()
  (interactive)
  (if (not (buffer-live-p outline-toc-source-buffer))
      (error "Source buffer is no longer valid."))
  (outline-toc-show-1 (current-buffer)
                      (outline-toc-find-target-noerror)
                      (current-column)))

(defun outline-toc-sync-start-timer ()
  (unless (timerp outline-toc-sync-timer)
    (setq outline-toc-sync-timer
          (run-with-timer 5 5 #'outline-toc-sync-function))))

(defun outline-toc-sync-function ()
  (let (continue)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (eq major-mode 'outline-toc-mode)
                   outline-toc-autosync
                   (buffer-live-p outline-toc-source-buffer))
          (setq continue t)  ; keep the timer running if at least one buffer is still syncing
          (if (/= (buffer-chars-modified-tick outline-toc-source-buffer)
                  outline-toc-source-modified-tick)
            (outline-toc-show-1 buffer
                                (outline-toc-find-target-noerror)
                                (current-column))))))
    (unless continue
      (cancel-timer outline-toc-sync-timer)
      (setq outline-toc-sync-timer nil))))

(defun outline-toc-toggle-autosync (&optional arg)
  "Toggle automatic synchronization of the toc.

If called interactively, toggle auto synchronization.  If the prefix argument is
positive, enable auto synchronization, and if it is zero or negative, disable it.

If called from Lisp, toggle automatic synchronization if ARG is `toggle'.  Enable if ARG
is nil, omitted, or a positive number.  Disable if ARG is a negative number."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 'toggle)))
  (let ((newstate (cond ((eq arg 'toggle) (not outline-toc-autosync))
                        ((and (numberp arg) (< arg 1)) nil)
                        (t t))))
    (unless (eq newstate outline-toc-autosync)
      (setq outline-toc-autosync newstate)
      (outline-toc-update-mode-name)
      (message "TOC autosync %s" (if outline-toc-autosync "enabled" "disabled"))
      (if outline-toc-autosync
          (outline-toc-sync-start-timer)))))


;;; follow mode

(defun outline-toc-toggle-follow-mode ()
  "When turned on, cursor motion will automatically display the matching source line."
  (interactive)
  (setq outline-toc-follow-mode (not outline-toc-follow-mode))
  (outline-toc-update-mode-name)
  (if outline-toc-follow-mode
      (add-hook 'post-command-hook 'outline-toc-follow-post-command-hook nil t)
    (remove-hook 'post-command-hook 'outline-toc-follow-post-command-hook t)))

(defun outline-toc-follow-post-command-hook ()
  (when (and outline-toc-follow-mode (not (= (pos-bol) outline-toc-follow-prev-line)))
    (setq outline-toc-follow-prev-line (pos-bol))
    (ignore-errors (outline-toc-display-heading))))


;;; follow toc links

(defalias 'outline-toc-mouse-goto-heading 'outline-toc-goto-heading)
(defun outline-toc-goto-heading (&optional event)
  "Go to the outline specified by EVENT, a mouse click.
If not invoked by a mouse click, go to the outline on the current line."
  (interactive (list last-nonmenu-event))
  (let* (;(buffer (when event (current-buffer)))
         (target
          (if (null event)
              (outline-toc-find-target)
            (with-current-buffer (window-buffer (posn-window (event-end event)))
              (save-excursion
                (goto-char (posn-point (event-end event)))
                (outline-toc-find-target))))))
    (pop-to-buffer outline-toc-source-buffer)
    (goto-char target)
    (run-hooks 'outline-toc-goto-heading-hook)))

(defun outline-toc-goto-heading-other-window ()
  "Goto the outline for the current line, in another window."
  (interactive)
  (let (;(curbuf (current-buffer))
        (pos (outline-toc-find-target)))
    (switch-to-buffer-other-window outline-toc-source-buffer)
    (goto-char pos)
    (run-hooks 'outline-toc-goto-heading-hook)))

(defun outline-toc-display-heading ()
  "Display in another window the outline for the current line."
  (interactive)
  (let ((pos (outline-toc-find-target))
        (display-buffer-overriding-action '(nil (inhibit-same-window . t)))
        window)
    (setq window (display-buffer outline-toc-source-buffer t))
    (save-selected-window
      (select-window window)
      (goto-char pos)
      (run-hooks 'outline-toc-goto-heading-hook))))

;;; toggle outline subtree visibility

(defun outline-toc-toggle-children ()
  "Show or hide the current subtree depending on its current state."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (outline-invisible-p (line-end-position)))
        (outline-hide-subtree)
      (outline-show-children 1000)
      (outline-show-entry))))
  

(provide 'outline-toc)
;;; outline-toc.el ends here
