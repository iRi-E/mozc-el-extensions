;; mozc-cursor-color.el --- Set cursor color corresponding to mozc's input state

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

;; This program adds cursor color feature to mozc-mode.

;;; Installation
;;
;; First, ensure you already can use mozc-mode.
;;
;; Save this file in a directory listed in load-parh, and put the
;; following code in your .emacs file.
;;
;; (require 'mozc-cursor-color)
;;
;; Cursor color will be changed to corresponding one to state of
;; mozc's input context.
;;
;; Tested on Emacs 24.


;; History:
;; 2013-11-05  S. Irie
;;         * Fix `mozc-current-input-mode' not buffer-local
;;         * Fix error handler not working properly
;;         * Fix typo in Commentary
;;         * Version 0.1.2
;;
;; 2013-10-27  S. Irie
;;         * Minor fixes
;;         * Version 0.1.1
;;
;; 2013-10-27  S. Irie
;;         * Initial release
;;         * Version 0.1.0


;;; Code

(require 'mozc)

(defvar mozc-cursor-color-version "0.1.2")

(defvar mozc-cursor-color-conflicts-list
  '(ac-completing))

(defvar mozc-cursor-color-alist
  '((direct . "blue")
    (read-only . "lime green")
    (hiragana . "red")
    (full-katakana . "goldenrod")
    (half-ascii . "dark orchid")
    (full-ascii . "orchid")
    (half-katakana . "dark goldenrod")))

(defvar mozc-cursor-color-timer-delay 0.1)

(defvar mozc-current-input-mode 'hiragana)
(make-variable-buffer-local 'mozc-current-input-mode)

(defadvice mozc-session-execute-command (after mozc-current-input-mode () activate)
  (if ad-return-value
      (let ((mode (mozc-protobuf-get ad-return-value 'mode)))
	(if mode
	    (setq mozc-current-input-mode mode)))))

(defvar mozc-cursor-color-timer nil)

(defun mozc-cursor-color-setup-timer (&optional cancel)
  (if (timerp mozc-cursor-color-timer)
	(cancel-timer mozc-cursor-color-timer))
  (setq mozc-cursor-color-timer
	(and (not cancel)
	     (run-with-idle-timer mozc-cursor-color-timer-delay t
				  'mozc-cursor-color-update))))

(defun mozc-cursor-color-update ()
  (condition-case err
      (catch 'exit
	(mapc (lambda (symbol)
		(if (and (boundp symbol)
			 (symbol-value symbol))
		    (throw 'exit nil)))
	      mozc-cursor-color-conflicts-list)
	(set-cursor-color
	 (or (cdr (assq (cond
			 ((and buffer-read-only
			       (not inhibit-read-only))
			  'read-only)
			 ((not mozc-mode)
			  'direct)
			 (t
			  mozc-current-input-mode))
			mozc-cursor-color-alist))
	     (frame-parameter nil 'foreground-color))))
    (error
     (message "error in mozc-cursor-color-update(): %S" err)
     (set-cursor-color (frame-parameter nil 'foreground-color))
     (mozc-cursor-color-setup-timer t)
     (remove-hook 'post-command-hook 'mozc-cursor-color-update)
     (message "mozc-cursor-color was disabled due to the error.  See \"*Messages*\" buffer."))))

(defun mozc-cursor-color-setup ()
  (interactive)
  (mozc-cursor-color-setup-timer)
  (remove-hook 'post-command-hook 'mozc-cursor-color-update)
  (add-hook 'post-command-hook 'mozc-cursor-color-update t))

(mozc-cursor-color-setup)


(provide 'mozc-cursor-color)

;;; mozc-cursor-color.el ends Here
