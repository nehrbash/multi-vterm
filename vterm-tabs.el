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
;; This started as a fork of multi-term.el but.
;;
;; Features that might be required by this library:
;;
;;  `vterm'
;;  `project'
;;  `tab-line'
;;; Code:
(require 'cl-lib)
(require 'vterm)
(require 'project)
(require 'tab-line)

(require 'svg-tabs)

(defgroup vterm-tabs nil
  "Multi term manager"
  :group 'vterm)

(defcustom vterm-tabs-buffer-name "shell"
  "The vterm buffer name."
  :type 'string
  :group 'vterm-tabs)

(defcustom vterm-tabs-dedicated-window-height 30
  "The height of the `vterm-tabs' dedicated window in rows."
  :type 'integer
  :group 'vterm-tabs)

(defcustom vterm-tabs-dedicated-window-height-percent nil
  "The height of the `vterm-tabs' dedicated window in percent of rows."
  :type 'integer
  :group 'vterm-tabs)

(defconst vterm-tabs-dedicated-window-height-percent-limits '(10 90)
  "The dedicated vterm buffer height boundaries in percent")

;; Variables
(defvar vterm-tabs-dedicated-window nil
  "The dedicated `vterm-tabs' window.")

(defvar vterm-tabs-dedicated-buffer nil
  "The dedicated `vterm-tabs' buffer.")

(defvar vterm-tabs-frame nil
  "The dedicated `vterm-tabs' frame.")


(defvar vterm-tabs-dedicated-buffer-name vterm-tabs-buffer-name
  "The dedicated vterm buffer name.")

(defvar vterm-tabs-buffer-list nil
  "The list of non-dedicated terminal buffers managed by `vterm-tabs'.")

;; Interactive Functions
;;;###autoload
(defun vterm-tabs ()
  "Create new vterm buffer."
  (interactive)
  (progn
	(set-window-dedicated-p vterm-tabs-dedicated-window nil)
	(let* ((default-directory "~/")
		 (vterm-buffer (vterm-tabs-get-buffer)))
    (setq vterm-tabs-buffer-list (nconc vterm-tabs-buffer-list (list vterm-buffer)))
    (set-buffer vterm-buffer)
    (vterm-tabs-internal)
    (switch-to-buffer vterm-buffer))
	(setq vterm-tabs-dedicated-window (selected-window))
	(setq vterm-tabs-dedicated-buffer (current-buffer))
	(setq vterm-tabs-dedicated-buffer-name (buffer-name))
	(set-window-dedicated-p vterm-tabs-dedicated-window t)))

;;;###autoload
(defun vterm-tabs-project ()
  "Create new vterm buffer."
  (interactive)
  (let* (
		  (project-root (or (project-current) `(transient . ,default-directory)))
		  )
	)
  (if (vterm-tabs-project-root)
      (if (buffer-live-p (get-buffer ))
          (if (string-equal (buffer-name (current-buffer)) (vterm-tabs-project-get-buffer-name))
              (delete-window (selected-window))
            (switch-to-buffer-other-window (vterm-tabs-project-get-buffer-name)))
        (let* ((vterm-buffer (vterm-tabs-get-buffer 'project))
               (vterm-tabs-buffer-list (nconc vterm-tabs-buffer-list (list vterm-buffer))))
          (set-buffer vterm-buffer)
          (vterm-tabs-internal)
          (switch-to-buffer-other-window vterm-buffer)))
    (message "This file is not in a project")))




(defun vterm-tabs-get-buffer (&optional dedicated-window)
  "Get vterm buffer name based on DEDICATED-WINDOW.
Optional argument DEDICATED-WINDOW: There are three types of DEDICATED-WINDOW: dedicated, project, default."
  (with-temp-buffer
    (let ((index 1)
          vterm-name)
      (cond ((eq dedicated-window 'dedicated) (setq vterm-name vterm-tabs-dedicated-buffer-name))
            ((eq dedicated-window 'project) (progn
                                              (setq vterm-name (vterm-tabs-project-get-buffer-name))
                                              (setq default-directory (vterm-tabs-project-root))))
            (t (progn
                 (while (buffer-live-p (get-buffer (vterm-tabs-format-buffer-index index)))
                   (setq index (1+ index)))
                 (setq vterm-name (vterm-tabs-format-buffer-index index)))))
      (let ((buffer (get-buffer vterm-name)))
        (if buffer
            buffer
          (let ((buffer (generate-new-buffer vterm-name)))
            (set-buffer buffer)
            (vterm-mode)
            buffer))))))

(defun vterm-tabs-project-root ()
  "Get `default-directory' for project using projectile or project.el."
  (unless (boundp 'vterm-tabs-projectile-installed-p)
    (setq vterm-tabs-projectile-installed-p (require 'projectile nil t)))
  (if vterm-tabs-projectile-installed-p
      (projectile-project-root)
    ))


(defun vterm-tabs-rename-buffer (name)
  "Rename vterm buffer to NAME."
  (interactive "MRename vterm buffer: ")
  (rename-buffer (vterm-tabs-format-buffer-name name)))

(defun vterm-tabs-format-buffer-name (name)
  "Format vterm buffer NAME."
  (format "%s - %s" vterm-tabs-buffer-name name))

(defun vterm-tabs-format-buffer-index (index)
  "Format vterm buffer name with INDEX."
  (format "%s: %s" vterm-tabs-buffer-name index))

(defun vterm-tabs-handle-close ()
  "Close current vterm buffer when `exit' from vterm buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))))))

(defun vterm-tabs-next (&optional offset)
  "Go to the next term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
  (interactive "P")
  (vterm-tabs-switch 'NEXT (or offset 1)))

(defun vterm-tabs-prev (&optional offset)
  "Go to the previous term buffer.
If OFFSET is `non-nil', will goto next term buffer with OFFSET."
  (interactive "P")
  (vterm-tabs-switch 'PREVIOUS (or offset 1)))

(defun vterm-tabs-switch (direction offset)
  "Internal `vterm-tabs' buffers switch function.
If DIRECTION is `NEXT', switch to the next term.
If DIRECTION `PREVIOUS', switch to the previous term.
Option OFFSET for skip OFFSET number term buffer."
  (unless (vterm-tabs-switch-internal direction offset)
    (vterm-tabs)))

;; Utility Functions
(defun vterm-tabs-internal ()
  "Internal handle for `vterm-tabs' buffer."
  (vterm-tabs-handle-close)
  (add-hook 'kill-buffer-hook #'vterm-tabs-kill-buffer-hook))

(defun vterm-tabs-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq major-mode 'vterm-mode)
    (let ((killed-buffer (current-buffer)))
      (setq vterm-tabs-buffer-list
            (delq killed-buffer vterm-tabs-buffer-list)))))


(defun vterm-tabs-dedicated-exist-p ()
  "Return non-nil if `vterm-tabs' dedicated window exists."
  (and (vterm-tabs-buffer-exist-p vterm-tabs-dedicated-buffer)
       (vterm-tabs-window-exist-p vterm-tabs-dedicated-window)))

(defun vterm-tabs-window-exist-p (window)
  "Return non-nil if WINDOW exist."
  (and window (window-live-p window)))

(defun vterm-tabs-buffer-exist-p (buffer)
  "Return non-nil if BUFFER exist.
Otherwise return nil."
  (and buffer (buffer-live-p buffer)))

(defun vterm-tabs-switch-internal (direction offset)
  "Internal `vterm-tabs' buffers switch function.
If DIRECTION is `NEXT', switch to the next term.
If DIRECTION `PREVIOUS', switch to the previous term.
Option OFFSET for skip OFFSET number term buffer."
  (when vterm-tabs-buffer-list
    (let ((buffer-list-len (length vterm-tabs-buffer-list))
          (my-index (cl-position (current-buffer) vterm-tabs-buffer-list)))
      (if my-index
          (let ((target-index (if (eq direction 'NEXT)
                                  (mod (+ my-index offset) buffer-list-len)
                                (mod (- my-index offset) buffer-list-len))))
            (switch-to-buffer (nth target-index vterm-tabs-buffer-list)))
        (switch-to-buffer (car vterm-tabs-buffer-list))))))

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

;;;###autoload
(define-minor-mode vterm-tabs-mode
  "Minor mode to handle tabs in vterm."
  :lighter nil
  (when vterm-tabs-mode
    (setq-local
     tab-line-tabs-function 'tab-line-tabs-buffer-groups
	 tab-line-tab-name-function #'svg-tabs--svg-line-tab-name-buffer
     tab-line-tabs-buffer-group-function 'vterm-tabs--tab-group)
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
  (when global-vterm-tabs-mode
    (advice-add 'vterm--get-color :override #'old-version-of-vterm--get-color)
	(advice-add 'tab-line-select-tab-buffer :around #'vterm-tabs--select-buffer)))

(defun vterm-tabs--select-buffer (orig-fun &rest args)
    (let ((window (selected-window)))
      (progn
		(set-window-dedicated-p window nil)
        (apply orig-fun args)
		(setq vterm-tabs-dedicated-window (selected-window))
		(setq vterm-tabs-dedicated-buffer (current-buffer))
		(setq vterm-tabs-dedicated-buffer-name (buffer-name))
        (set-window-dedicated-p (window) t)
		)))

(provide 'vterm-tabs)
;;; vterm-tabs.el ends here
