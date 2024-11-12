;;; vterm-tabs.el --- vterm multiplexer -*- lexical-binding: t; -*-
;;
;; Authors: Stephen Nehrbass
;; Keywords: terminals, processes
;; Version: 1.0
;; Package-Requires: ((emacs "26.3") (vterm "0.0") (project "0.3.0"))

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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
;; Managing multiple vterm buffers in Emacs
;; This started as a fork of multi-term.el but It doesn't really resemble the
;; original anymore.
;;
;; packages not built-in
;;  `vterm'
;;  `svg-tabs'
;;; Code:
(require 'cl-lib)
(require 'vterm)
(require 'project)
(require 'tab-line)
(require 'svg-tabs)
(require 'compile)

(defgroup vterm-tabs nil
  "Multi term manager"
  :group 'vterm)

(defcustom vterm-tabs-buffer-name "shell"
  "The vterm buffer name."
  :type 'string
  :group 'vterm-tabs)

(defvar vterm-tabs-window nil
  "Window displaying the vterm sidebar.")

(defvar vterm-tabs-buffer-list nil
  "The list of non-dedicated terminal buffers managed by `vterm-tabs'.")

(defvar vterm-tabs-last-buffer nil
  "The last accessed vterm buffer.")

;; compile buffer from sanityinc
(defvar vterm-tabs--last-compilation-buffer nil
	"The last buffer in which compilation took place.")

(defun vterm-tabs--save-compilation-buffer (&rest _)
	"Save the compilation buffer to find it later."
	(setq vterm-tabs--last-compilation-buffer next-error-last-buffer))

(defun vterm-tabs--find-prev-compilation (orig &optional edit-command)
  "Find the previous compilation buffer, if present, and recompile there.
If EDIT-COMMAND is nil and we are not in a compilation mode,
it attempts to use `vterm-tabs--last-compilation-buffer`."
  (if (and (null edit-command)
           (not (derived-mode-p 'compilation-mode))
           vterm-tabs--last-compilation-buffer
           (buffer-live-p (get-buffer vterm-tabs--last-compilation-buffer)))
      (let ((compilation-buffer vterm-tabs--last-compilation-buffer))
        (if (window-live-p vterm-tabs-window)
            (progn
              (set-window-buffer vterm-tabs-window compilation-buffer)
              (select-window vterm-tabs-window)
              (with-current-buffer compilation-buffer
                (funcall orig edit-command)))
          (with-current-buffer compilation-buffer
            (funcall orig edit-command))))
    ;; Fallback to the original behavior if no previous compilation buffer
    (funcall orig edit-command)))

(defun vterm-tabs--colourise-compilation-buffer ()
	(when (eq major-mode 'compilation-mode)
	  (ansi-color-apply-on-region compilation-filter-start (point-max))))

(defun vterm-tabs-create-or-switch ()
  "Create a new vterm buffer or switch to an existing one, reusing the window."
  (interactive)
  (let ((buffer (generate-new-buffer (generate-new-buffer-name vterm-tabs-buffer-name))))
    (with-current-buffer buffer
      (vterm-mode)
      (add-hook 'kill-buffer-hook #'vterm-tabs-remove-buffer nil t)
      (setq vterm-tabs-buffer-list (append vterm-tabs-buffer-list (list buffer))))
    (vterm-tabs-switch buffer)))

(defun vterm-tabs-remove-buffer ()
  "Remove the current buffer from `vterm-tabs-buffer-list` when it's killed."
  (setq vterm-tabs-buffer-list (delq (current-buffer) vterm-tabs-buffer-list))
  (when (eq vterm-tabs-last-buffer (current-buffer))
    (setq vterm-tabs-last-buffer nil)))

;;;###autoload
(defun vterm-tabs-toggle ()
  "Toggle the visibility of the vterm sidebar."
  (interactive)
  (if (and vterm-tabs-window (window-live-p vterm-tabs-window))
      (progn
		(setq vterm-tabs-last-buffer (window-buffer vterm-tabs-window))
        (delete-window vterm-tabs-window)
        (setq vterm-tabs-window nil))
    (if (and vterm-tabs-last-buffer (buffer-live-p vterm-tabs-last-buffer))
        (vterm-tabs-switch vterm-tabs-last-buffer)
      (vterm-tabs-create-or-switch))))

(defun vterm-tabs-create-at-directory (directory)
  "Create a new vterm buffer in the specified DIRECTORY and switch to it."
  (let ((default-directory directory))
    (vterm-tabs-create-or-switch)))

;;;###autoload
(defun vterm-tabs-project ()
  "Create a new vterm buffer at the current project's root."
  (interactive)
  (let* ((project (project-current))
         (project-root (if project
                           (project-root project)
                         (user-error "No project found"))))
    (vterm-tabs-create-at-directory project-root)))

;;;###autoload
(defun vterm-tabs-home ()
  "Create a new vterm buffer at the user's home directory."
  (interactive)
  (vterm-tabs-create-at-directory (expand-file-name "~/")))

(defun vterm-tabs-rename-buffer (name)
  "Rename vterm buffer to NAME."
  (interactive "MRename vterm buffer: ")
  (rename-buffer (format "%s - %s" vterm-tabs-buffer-name name)))

(defun vterm-tabs-next (&optional offset)
  "Go to the next term buffer.
If OFFSET is `non-nil', will go to the next term buffer with OFFSET."
  (interactive "P")
  (vterm-tabs-switch-internal 'NEXT (or offset 1)))

(defun vterm-tabs-prev (&optional offset)
  "Go to the previous term buffer.
If OFFSET is `non-nil', will go to the previous term buffer with OFFSET."
  (interactive "P")
  (vterm-tabs-switch-internal 'PREVIOUS (or offset 1)))

(defun vterm-tabs-switch-internal (direction offset)
  "Internal `vterm-tabs' buffer switch function.
If DIRECTION is `NEXT', switch to the next term.
If DIRECTION is `PREVIOUS', switch to the previous term.
OPTION OFFSET for skipping OFFSET number of term buffers."
  (let ((all-buffers (vterm-tabs--all-buffers)))
    (when all-buffers
      (let* ((buffer-list-len (length all-buffers))
             (my-index (cl-position (current-buffer) all-buffers))
             (target-index (if my-index
                               (if (eq direction 'NEXT)
                                   (mod (+ my-index offset) buffer-list-len)
                                 (mod (- my-index offset) buffer-list-len))
                             0))) ;; Default to first buffer if not found
        (vterm-tabs-switch (nth target-index all-buffers))))))

(defun vterm-tabs-switch (buffer)
  "Switch to a vterm BUFFER, reusing the existing window."
  (setq vterm-tabs-last-buffer buffer)
  (if (window-live-p vterm-tabs-window)
      (let ((was-dedicated (window-dedicated-p vterm-tabs-window)))
        (set-window-dedicated-p vterm-tabs-window nil)
        (set-window-buffer vterm-tabs-window buffer)
        (set-window-dedicated-p vterm-tabs-window was-dedicated)
        (select-window vterm-tabs-window))
    (setq vterm-tabs-window (display-buffer-in-side-window buffer '((side . bottom))))
    (set-window-dedicated-p vterm-tabs-window t)
    (set-window-parameter vterm-tabs-window 'no-other-window t)
    (select-window vterm-tabs-window)))


;; tab line stuff
(defun vterm-tabs--tab-group (buffer)
  "Group buffers by major mode.
  Returns a single group name as a string for buffers with major modes
  flymake-project-diagnostics-mode, compilation-mode, and vterm-mode."
  (with-current-buffer buffer
    (when (or (derived-mode-p 'flymake-project-diagnostics-mode)
			  (derived-mode-p 'compilation-mode)
			  (derived-mode-p 'vterm-mode))
	  "🦥")))


;; TODO(SN): remove when this is fixed.
;;;###autoload
(defun old-version-of-vterm--get-color (index &rest args)
  "This is the old version before it was broken by commit
https://github.com/akermu/emacs-libvterm/commit/e96c53f5035c841b20937b65142498bd8e161a40.
Re-introducing the old version fixes auto-dim-other-buffers for vterm buffers."
  (cond
   ((and (>= index 0) (< index 16))
    (face-foreground
     (elt vterm-color-palette index)
     nil 'default))
   ((= index -11)
    (face-foreground 'vterm-color-underline nil 'default))
   ((= index -12)
    (face-background 'vterm-color-inverse-video nil 'default))
   (t
    nil)))

;;;###autoload
(defun sn/vterm-new-tab ()
	"Create a new tab for the toggled vterm buffers"
	(interactive)
    (let ((default-directory "~/"))
	  (set-window-dedicated-p vterm-tabs-dedicated-window nil)
	  (let* ((vterm-buffer (vterm-tabs-get-buffer)))
		(setq vterm-tabs-buffer-list (nconc vterm-tabs-buffer-list (list vterm-buffer)))
		(set-buffer vterm-buffer)
		(vterm-tabs-internal)
		(switch-to-buffer vterm-buffer))
	  (setq vterm-tabs-dedicated-window (selected-window))
	  (setq vterm-tabs-dedicated-buffer (current-buffer))
	  (setq vterm-tabs-dedicated-buffer-name (buffer-name))
	  (set-window-dedicated-p vterm-tabs-dedicated-window t)))


(defun vterm-tabs--all-buffers ()
  "Return a list of buffers to display during vterm-tabs-mode.
Include the last accessed vterm buffer, any active vterm buffers,
the last compilation buffer, and possibly a magit status buffer."

  (let ((buffers
         (append
          ;; Include vterm buffers
          vterm-tabs-buffer-list
          
          ;; Include last accessed vterm buffer if it's live and not already in the list
          (when (and vterm-tabs-last-buffer
                     (buffer-live-p vterm-tabs-last-buffer)
                     (not (memq vterm-tabs-last-buffer vterm-tabs-buffer-list)))
            (list vterm-tabs-last-buffer))
          
          ;; Include the last compilation buffer if it's live
          (when (and vterm-tabs--last-compilation-buffer
                     (buffer-live-p vterm-tabs--last-compilation-buffer))
            (list vterm-tabs--last-compilation-buffer))
          
          ;; Fallback: create and set the compilation buffer if it's nil
          (unless (and vterm-tabs--last-compilation-buffer
                       (buffer-live-p vterm-tabs--last-compilation-buffer))
            ;; Create buffer and set it to vterm-tabs--last-compilation-buffer
            (let ((compilation-buffer (get-buffer-create "*compilation*")))
              (setq vterm-tabs--last-compilation-buffer compilation-buffer)
              (list compilation-buffer)))
          
          ;; Optionally include the magit status buffer for the current project
          (let ((magit-buffer (and (fboundp 'magit-status-buffer)
                                   (magit-status-buffer))))
            (when (and magit-buffer (buffer-live-p magit-buffer))
              (list magit-buffer))))))
    ;; Return the list of buffers
    buffers))

;;;###autoload
(define-minor-mode vterm-tabs-mode
  "Minor mode to handle tabs in vterm."
  :lighter nil
  (when vterm-tabs-mode
    (setq-local
     tab-line-tabs-function 'vterm-tabs--all-buffers
	 tab-line-tab-name-function #'svg-tabs--svg-line-tab-name-buffer)
    (tab-line-mode 1)))


(defvar vterm-tabs-mode--exclude nil
  "List of regex patterns to exclude buffers from vterm-tabs-mode.")

(defun vterm-tabs-mode--on ()
  "Turn on vterm-tabs-mode for certain major modes, excluding buffers matching patterns in `vterm-tabs-mode--exclude`."
  (when (derived-mode-p 'compilation-mode 'vterm-mode 'flymake-project-diagnostics-mode)
    (vterm-tabs-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-vterm-tabs-mode
  vterm-tabs-mode vterm-tabs-mode--on
  :group 'vterm-tabs
  (if global-vterm-tabs-mode
	(progn
      (advice-add 'vterm--get-color :override #'old-version-of-vterm--get-color)
	  (advice-add 'tab-line-select-tab-buffer :around #'vterm-tabs--select-buffer)
	  (advice-add 'compilation-start :after 'vterm-tabs--save-compilation-buffer)
	  (advice-add 'recompile :around 'vterm-tabs--find-prev-compilation)
	  (add-hook 'compilation-filter-hook 'vterm-tabs--colourise-compilation-buffer))
	(progn
	  (advice-remove 'vterm--get-color  #'old-version-of-vterm--get-color)
	  (advice-remove 'tab-line-select-tab-buffer  #'vterm-tabs--select-buffer)
	  (advice-remove 'compilation-start  'vterm-tabs--save-compilation-buffer)
	  (remove-hook 'compilation-filter-hook 'vterm-tabs--colourise-compilation-buffer))))

(defun vterm-tabs--select-buffer (orig-fun &rest args)
    (let ((window (selected-window)))
      (progn
		(set-window-dedicated-p window nil)
        (apply orig-fun args)
		(setq vterm-tabs-dedicated-window (selected-window))
		(setq vterm-tabs-dedicated-buffer (current-buffer))
		(setq vterm-tabs-dedicated-buffer-name (buffer-name))
        (set-window-dedicated-p (window) t))))

(provide 'vterm-tabs)
;;; vterm-tabs.el ends here
