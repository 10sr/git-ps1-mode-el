;;; git-ps1-mode.el --- Global minor-mode to print __git_ps1

;; Author: 10sr <8slashes+el [at] gmail [dot] com>
;; URL: https://github.com/10sr/git-ps1-mode-el
;; Version: 0.1.0
;; Keywords: utility mode-line git

;; Contributor: acple <silentsphere110@gmail.com>

;;; Code:

(defvar git-ps1-mode-string "[......]"
  "String to show in mode-line.")

(defvar git-ps1-mode-process nil
  "Existing process object or nil.")

;; make local-variable
(make-variable-buffer-local 'git-ps1-mode-string)
(make-variable-buffer-local 'git-ps1-mode-process)


(defun git-ps1-mode-schedule-update (buffer &optional force)
  "Register process execution timer to run in BUFFER.
"
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
             (setq git-ps1-mode-string "[no-branch]"))
            ((string-match "^fatal" output)
             (setq git-ps1-mode-string "[no-repo]"))
            (t
             (setq git-ps1-mode-string
                   (format "[%s]" (substring output 11 -1)))))
      (force-mode-line-update))))

(defun git-ps1-mode-clear-process (process state)
  "Clear exitted process.
This function is passed as an argument for `set-process-sentinel': see
document of that function for details about PROCESS and STATE."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (setq git-ps1-mode-process nil))))


;; ===hook用関数定義============================================

;; hook#1 after-change-major-mode-hook, after-save-hook
(defun git-ps1-mode-update-current ()
  (unless (minibufferp (current-buffer))
    (git-ps1-mode-schedule-update (current-buffer) t)))

;; hook#2 select-window-functions
(defun git-ps1-mode-update-when-select-window
  (before-win after-win)
  (unless (minibufferp (window-buffer after-win))
    (git-ps1-mode-schedule-update (window-buffer after-win))))

;; hook#3 set-selected-window-buffer-functions
(defun git-ps1-mode-update-when-set-window-buffer
  (before-buf win after-buf)
  (unless (minibufferp after-buf)
    (git-ps1-mode-schedule-update after-buf)))


;; ===マイナーモード定義========================================

(defun git-ps1-mode-enable ()
  (setcar (or (member '(vc-mode vc-mode) mode-line-format)
              (list nil))
          'git-ps1-mode-string)
  (git-ps1-mode-update-current)
  (add-hook 'after-change-major-mode-hook
            'git-ps1-mode-update-current)
  (add-hook 'after-save-hook
            'git-ps1-mode-update-current)
  (add-hook 'select-window-functions
            'git-ps1-mode-update-when-select-window)
  (add-hook 'set-selected-window-buffer-functions
            'git-ps1-mode-update-when-set-window-buffer))

(defun git-ps1-mode-disable ()
  (setcar (or (memq 'git-ps1-mode-string mode-line-format)
              (list nil))
          '(vc-mode vc-mode))
  (remove-hook 'after-change-major-mode-hook
               'git-ps1-mode-update-current)
  (remove-hook 'after-save-hook
               'git-ps1-mode-update-current)
  (remove-hook 'select-window-functions
               'git-ps1-mode-update-when-select-window)
  (remove-hook 'set-selected-window-buffer-functions
               'git-ps1-mode-update-when-set-window-buffer))

(define-minor-mode git-ps1-mode-mode
  "[git-branch]"
  :group 'git-ps1-mode
  :global t
  (if git-ps1-mode-mode
      (git-ps1-mode-enable)
    (git-ps1-mode-disable))
  (force-mode-line-update t))

(provide 'git-ps1-mode)

;;; git-ps1-mode.el ends here
