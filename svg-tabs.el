;;; svg-tabs.el --- vterm multiplexer -*- lexical-binding: t; -*-

(require 'svg-tag-mode)
(require 'tab-line)

(defun svg-tabs--svg-line-tab-name-buffer (buffer &optional _buffers)
  "Create the SVG representation of BUFFER's tab in the tab line."
  (let* ((name (buffer-name buffer))
		 (current (eq (current-buffer) buffer)))
	(propertize
	 name
	 'display
	 (svg-tag-make
	  name
	  :face (if current 'tab-line-tab-current 'tab-line-tab-inactive)
	  :inverse current
	  :radius 8
	  :margin 0
	  :scale 2
	   :font-weight 'bold))))

;;;###autoload
(setq
  tab-line-tab-name-function #'svg-tabs--svg-line-tab-name-buffer
  tab-line-new-button-show nil
  tab-line-close-button-show nil)

(provide 'svg-tabs)
;;; svg-tabs.el ends here
