;;; mozc-isearch.el --- Use mozc-mode in isearch-mode

;; Copyright (C) 2013 S. Irie

;; Author: S. Irie, October 2013
;; Keywords: mule, multilingual, input method

;; Everyone is permitted to copy and distribute verbatim or modified
;; copies of this license document, and changing it is allowed as long
;; as the name is changed.

;;            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

;;  0. You just DO WHAT THE FUCK YOU WANT TO.


;;; Commentary

;; This program allows you to use mozc-mode for the incremental search.

;;; Installation
;;
;; First, ensure you already can use mozc-mode.
;;
;; Save this file in a directory listed in load-parh, and put the
;; following code in your .emacs file.
;;
;; (require 'mozc-isearch)
;;
;; That's all.  Now you can do incremental search in Japanese.
;;
;; Tested on Emacs 24.


;; History:
;; 2013-11-06  S. Irie
;;         * Add option `mozc-isearch-use-workaround'
;;         * Rename some function and advice
;;         * Version 0.3.0
;;
;; 2013-11-05  S. Irie
;;         * `isearch-delete-char' deletes single character from search string
;;         * Avoid possible error
;;         * Fix typo in Commentary
;;         * Version 0.2.0
;;
;; 2013-10-27  S. Irie
;;         * Initial release
;;         * Version 0.1.0


;;; Code

(require 'mozc)

(defvar mozc-isearch-version "0.3.0")

(defvar mozc-isearch-use-workaround t)

(defvar mozc-preedit-empty-hook nil)
(defvar mozc-mode-end-hook nil)

(defadvice mozc-handle-event (around mozc-preedit-empty-hook () activate)
  (let ((mozc-preedit-is-empty nil)) ; really ugly hack!
    ad-do-it
    (if mozc-preedit-is-empty
	(run-hooks 'mozc-preedit-empty-hook))))

(defadvice mozc-send-key-event (after mozc-preedit-is-empty () activate)
  (if (and (not (and ad-return-value
		     (mozc-protobuf-get ad-return-value 'preedit)))
	   (boundp 'mozc-preedit-is-empty))
      (setq mozc-preedit-is-empty t)))

(defadvice mozc-mode (after mozc-mode-end-hook () activate)
  (if (not mozc-mode)
      (run-hooks 'mozc-mode-end-hook)))

(defun mozc-mode-save-minibuffer-state ()
  (if (boundp 'mozc-mode-in-minibuffer)
      (setq mozc-mode-in-minibuffer mozc-mode)))

(defadvice isearch-process-search-multibyte-characters
  (around mozc-isearch-process-search-characters () activate)
  (if (and (equal current-input-method "japanese-mozc")
	   (eq this-command 'isearch-printing-char))
      (let ((mozc-mode-in-minibuffer mozc-mode)
	    str)
	(let ((overriding-terminal-local-map nil)
	      (prompt (isearch-message-prefix))
	      (minibuffer-local-map isearch-minibuffer-local-map)
	      (mozc-preedit-empty-hook '(exit-minibuffer))
	      (mozc-mode-end-hook '(exit-minibuffer))
	      (minibuffer-exit-hook '(mozc-mode-save-minibuffer-state))
	      junk-hist)
	  (setq unread-command-events (cons last-char unread-command-events)
		str (substring
		     (read-string prompt isearch-string 'junk-hist nil t)
		     (length isearch-string))))
	(if (not mozc-mode-in-minibuffer)
	    (deactivate-input-method))
	(if (and str (> (length str) 0))
	    (let ((unread-command-events nil))
	      (dotimes (i (length str))
		(let ((ch (string (aref str i))))
		  (isearch-process-search-string ch ch))))
	  (isearch-update)))
    ad-do-it))

(defun mozc-isearch-setup ()
  (interactive)
  (substitute-key-definition
   'toggle-input-method 'isearch-toggle-input-method isearch-mode-map global-map))

(add-hook 'isearch-mode-hook 'mozc-isearch-setup)


;; Workaround for bug in mozc.el

(defun mozc-mode-ensure-active ()
  (if (and (equal current-input-method "japanese-mozc")
	   (not mozc-mode))
      (mozc-mode 1)))

(defun mozc-isearch-workaround-setup ()
  (when mozc-isearch-use-workaround
    (add-hook 'minibuffer-setup-hook 'mozc-mode-ensure-active)))

(add-hook 'after-init-hook 'mozc-isearch-workaround-setup)


(provide 'mozc-isearch)

;;; mozc-isearch.el ends here
