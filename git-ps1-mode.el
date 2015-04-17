;;; git-ps1-mode.el --- Global minor-mode to print __git_ps1

;; Author: 10sr <8slashes+el [at] gmail [dot] com>
;; URL: https://github.com/10sr/git-ps1-mode-el
;; Version: 0.1.0
;; Keywords: utility mode-line git

;; Contributor: acple <silentsphere110@gmail.com>

;;; Commentary:

;; Global minor-mode to print __git_ps1 in mode-line.

;;; Code:

(defvar git-ps1-mode-process nil
  "Existing process object or nil.")

(defvar git-ps1-mode-line-text
  ""
  "Lighter text for `git-ps1-mode'.")

;; make local-variable
(make-variable-buffer-local 'git-ps1-mode-line-text)
(make-variable-buffer-local 'git-ps1-mode-process)


(defun git-ps1-mode-schedule-update (buffer &optional force)
  "Register process execution timer.
Arguments BUFFER and FORCE will be passed to `git-ps1-mode-run-proess'."
  (run-with-idle-timer
   0.0 nil #'git-ps1-mode-run-process buffer force))

(defun git-ps1-mode-run-process (buffer force)
  "Run git process in BUFFER and get branch name.
Set FORCE to non-nil to skip buffer check."
  (when (or (and force (buffer-live-p buffer))
            (eq buffer (current-buffer)))
    (with-current-buffer buffer
      (unless git-ps1-mode-process
        (let ((process-connection-type nil))
          (setq git-ps1-mode-process
                (start-process "git-ps1-mode" buffer
                               "git" "symbolic-ref" "HEAD"))
          (set-process-filter git-ps1-mode-process
                              'git-ps1-mode-update-modeline)
          (set-process-sentinel git-ps1-mode-process
                                'git-ps1-mode-clear-process)
          (set-process-query-on-exit-flag git-ps1-mode-process
                                          nil))))))

(defun git-ps1-mode-update-modeline (process output)
  "Format output of `git-ps1-mode-run-process' and update modeline.
This function is passed as an argument for `set-process-filter': see
document of that function for details about PROCESS and OUTPUT."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (cond ((string= "fatal: ref HEAD is not a symbolic ref"
                      (substring output 0 -1))
             (setq git-ps1-mode-line-text " [no-branch]"))
            ((string-match "^fatal" output)
             (setq git-ps1-mode-line-text " [no-repo]"))
            (t
             (setq git-ps1-mode-line-text
                   (format " [%s]" (substring output 11 -1)))))
      (force-mode-line-update))))

(defun git-ps1-mode-clear-process (process state)
  "Clear exitted process.
This function is passed as an argument for `set-process-sentinel': see
document of that function for details about PROCESS and STATE."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (setq git-ps1-mode-process nil))))


;; Functions for hooks

;; hook#1 after-change-major-mode-hook, after-save-hook
(defun git-ps1-mode-update-current ()
  "Update status text immediately."
  (interactive)
  (unless (minibufferp (current-buffer))
    (git-ps1-mode-schedule-update (current-buffer) t)))

;; hook#2 select-window-functions
(defun git-ps1-mode-update-when-select-window (before-win after-win)
  "Update status text immediately.
BEFORE-WIN and AFTER-WIN will be passed by `select-window-functions' hook."
  (unless (minibufferp (window-buffer after-win))
    (git-ps1-mode-schedule-update (window-buffer after-win))))

;; hook#3 set-selected-window-buffer-functions
(defun git-ps1-mode-update-when-set-window-buffer (before-buf win after-buf)
  "Update status text immediately.
BEFORE-BUF, WIN and AFTER-BUF will be passed by
`set-selected-window-buffer-functions' hook."
  (unless (minibufferp after-buf)
    (git-ps1-mode-schedule-update after-buf)))


;; Minor-mode

(define-minor-mode git-ps1-mode
  "Minor-mode to print __git_ps1."
  :global t
  :lighter (:eval git-ps1-mode-line-text)
  (if git-ps1-mode
      (progn
        (git-ps1-mode-update-current)
        (add-hook 'after-change-major-mode-hook
                  'git-ps1-mode-update-current)
        (add-hook 'after-save-hook
                  'git-ps1-mode-update-current)
        (add-hook 'select-window-functions
                  'git-ps1-mode-update-when-select-window)
        (add-hook 'set-selected-window-buffer-functions
                  'git-ps1-mode-update-when-set-window-buffer))
    (remove-hook 'after-change-major-mode-hook
                 'git-ps1-mode-update-current)
    (remove-hook 'after-save-hook
                 'git-ps1-mode-update-current)
    (remove-hook 'select-window-functions
                 'git-ps1-mode-update-when-select-window)
    (remove-hook 'set-selected-window-buffer-functions
                 'git-ps1-mode-update-when-set-window-buffer))
  (force-mode-line-update t))

(provide 'git-ps1-mode)

;;; git-ps1-mode.el ends here
