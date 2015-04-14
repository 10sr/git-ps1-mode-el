;;; git-ps1-mode.el --- Global minor-mode to print __git_ps1

;; Author: 10sr <8slashes+el [at] gmail [dot] com>
;; URL: https://github.com/10sr/git-ps1-mode-el
;; Version: 0.1.1
;; Keywords: utility mode-line git

;; Contributor: acple <silentsphere110@gmail.com>

;; This file is not part of GNU Emacs.

;; Copyright (C) 2015 by 10sr <8slashes+el [at] gmail [dot] com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.


;;; Commentary:

;; Global minor-mode to print `__git_ps1` in mode-line.

;; This minor-mode will print current `git` status in Emacs mode-line as a
;; mode-name. Status text will be generated using `__git_ps1`, which is usually
;; defined in `"git-prompt.sh"`. By default, the text should be like
;; `"[GIT:master *]"`.


;; User Configuration Variables
;; ----------------------------

;; * `git-ps1-mode-lighter-text-format`

;;   Format string for `git-ps1-mode` lighter (mode-name). By default it is set to
;;   `" [GIT:%s]"`.

;; * `git-ps1-mode-ps1-file-candidates-list`

;;   List of candidates that may contain `__git_ps1` definition.
;;   At the first invocation, `git-ps1-mode` searchs these files for `__git_ps1`
;;   definition, and set the first file to `git-ps1-mode-ps1-file`.

;; * `git-ps1-mode-showdirtystate`
;; * `git-ps1-mode-showstashstate`
;; * `git-ps1-mode-showuntrackedfiles`
;; * `git-ps1-mode-showupstream`

;;   Values for `GIT_PS1_SHOWDIRTYSTATE`, `GIT_PS1_SHOWSTASHSTATE`,
;;   `GIT_PS1_SHOWUNTERACKEDFILES` and `GIT_PS1_SHOWUPSTREAM` respectively.
;;   These variables configure output of `__git_ps1`: see document in
;;   "git-prompt.sh" file for details.

;; * `git-ps1-mode-idle-interval`

;;   If Emacs is idle for this seconds, this mode will update the status text.
;;   By default it is set to `2`.


;; TODO: Instruct users to set git-ps1-mode-ps1-file
;; TODO: Use same status text if `git rev-parse --show-toplevel` is same

;;; Code:


(defvar git-ps1-mode-ps1-file-candidates-list
  '(
    "/Applications/Xcode.app/Contents/Developer/usr/share/git-core/git-prompt.sh"
    "/usr/share/git/completion/git-prompt.sh"
    "/opt/local/share/doc/git-core/contrib/completion/git-prompt.sh"
    "/etc/bash_completion.d/git"
    "/etc/bash_completion.d/git-prompt"
    "/opt/local/share/git-core/git-prompt.sh"
    "/opt/local/etc/bash_completion.d/git"
    )
  "List of candidates that may contain \"__git_ps1\" definition.
This list will be loaded at the first time when `git-ps1-mode' is enabled.")

(defvar git-ps1-mode-ps1-file
  nil
  "File path that contains \"__git_ps1\" definition.
Usually this will be searched automatically by `git-ps1-mode-find-ps1-file'
so usually you do not need to set this explicitly.
Instead, add to `git-ps1-mode-ps1-file-candidates-list' if you want to check
other files.")

;; variables to configure __git_ps1
(defvar git-ps1-mode-showdirtystate
  (or (getenv "GIT_PS1_SHOWDIRTYSTATE")
      "")
  "Value of GIT_PS1_SHOWDIRTYSTATE when running __git_ps1.")

(defvar git-ps1-mode-showstashstate
  (or (getenv "GIT_PS1_SHOWSTASHSTATE")
      "")
  "Value of GIT_PS1_SHOWSTASHSTATE when running __git_ps1.")

(defvar git-ps1-mode-showuntrackedfiles
  (or (getenv "GIT_PS1_SHOWUNTRACKEDFILES")
      "")
  "Value of GIT_PS1_SHOWUNTRACKEDFILES when running __git_ps1.")

(defvar git-ps1-mode-showupstream
  (or (getenv "GIT_PS1_SHOWUPSTREAM")
      "")
  "Value of GIT_PS1_SHOWUPSTREAM when running __git_ps1.")



(defvar git-ps1-mode-process nil
  "Existing process object or nil.")

(defvar git-ps1-mode-lighter-text
  ""
  "Lighter text for `git-ps1-mode'.  This variable is for internal usage.")

(defvar git-ps1-mode-lighter-text-format " [GIT:%s]"
  "Format for `git-ps1-mode' lighter.
String \"%s\" will be replaced with the output of \"__git_ps1 %s\".")

;; make local-variable
(make-variable-buffer-local 'git-ps1-mode-lighter-text)
(make-variable-buffer-local 'git-ps1-mode-process)


(defvar git-ps1-mode-idle-interval 2
  "If Emacs is idle for this seconds `git-ps1-mode' will update lighter text.")

(defvar git-ps1-mode-idle-timer-object nil
  "Idle timer object returned from `run-with-idle-timer'.")



;; Functions

(defun git-ps1-mode-ps1-available-p (f)
  "Return F if F exists and it contain function \"__git_ps1\"."
  (and (file-readable-p f)
       (with-temp-buffer
         (insert ". " f "; "
                 "__git_ps1 %s;")
         (= 0 (shell-command-on-region (point-min)
                                       (point-max)
                                       "bash -s"
                                       nil
                                       t)))
       f))

(defun git-ps1-mode-find-ps1-file (&optional list)
  "Find file that contain \"__git_ps1\" definition from LIST.
This function returns the path of the first file foundor nil if none.  If LIST
 if omitted `git-ps1-mode-ps1-file-candidates-list' will be used."
  (let ((l (or list
               git-ps1-mode-ps1-file-candidates-list)))
    (and l
         (if (git-ps1-mode-ps1-available-p (car l))
             (car l)
           (and (cdr l)
                (git-ps1-mode-find-ps1-file (cdr l)))))))


(defun git-ps1-mode-schedule-update (buffer &optional force)
  "Register process execution timer.
Arguments BUFFER and FORCE will be passed to `git-ps1-mode-run-proess'."
  (when git-ps1-mode-ps1-file
    (run-with-idle-timer
     0.0 nil #'git-ps1-mode-run-process buffer force)))

(defun git-ps1-mode-run-process (buffer force)
  "Run git process in BUFFER and get branch name.
Set FORCE to non-nil to skip buffer check."
  (when (or (and force (buffer-live-p buffer))
            (eq buffer (current-buffer)))
    (with-current-buffer buffer
      (unless git-ps1-mode-process
        (let ((process-environment `(,(concat "GIT_PS1_SHOWDIRTYSTATE="
                                              git-ps1-mode-showdirtystate)
                                     ,(concat "GIT_PS1_SHOWSTASHSTATE="
                                              git-ps1-mode-showstashstate)
                                     ,(concat "GIT_PS1_SHOWUNTRACKEDFILES="
                                              git-ps1-mode-showuntrackedfiles)
                                     ,(concat "GIT_PS1_SHOWUPSTREAM="
                                              git-ps1-mode-showupstream)
                                     ,@process-environment))
              (process-connection-type nil))
          (setq git-ps1-mode-process
                (start-process "git-ps1-mode" buffer
                               ;; TODO: parameterize bash executable
                               "bash" "-s"))
          (set-process-filter git-ps1-mode-process
                              'git-ps1-mode-update-modeline)
          (set-process-sentinel git-ps1-mode-process
                                'git-ps1-mode-clear-process)
          (set-process-query-on-exit-flag git-ps1-mode-process
                                          nil)
          (process-send-string git-ps1-mode-process
                               (format ". \"%s\"; __git_ps1 %s"
                                       git-ps1-mode-ps1-file
                                       "%s"))
          (process-send-eof git-ps1-mode-process))))))

(defun git-ps1-mode-update-modeline (process output)
  "Format output of `git-ps1-mode-run-process' and update modeline.
This function is passed as an argument for `set-process-filter': see
document of that function for details about PROCESS and OUTPUT."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (setq git-ps1-mode-lighter-text
            (format git-ps1-mode-lighter-text-format
                    output))
      (force-mode-line-update))))

(defun git-ps1-mode-clear-process (process state)
  "Clear exitted process.
This function is passed as an argument for `set-process-sentinel': see
document of that function for details about PROCESS and STATE."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (setq git-ps1-mode-process nil))))



;; Functions for hooks

;; hook#1 after-change-major-mode-hook, after-save-hook,
;; window-configuration-change-hook, run-with-idle-timer
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

;;;###autoload
(define-minor-mode git-ps1-mode
  "Minor-mode to print __git_ps1."
  :global t
  :lighter (:eval git-ps1-mode-lighter-text)
  (if git-ps1-mode
      (progn
        (unless git-ps1-mode-ps1-file
          (error "`git-ps1-mode-ps1-file' is not set"))
        (unless (file-readable-p git-ps1-mode-ps1-file)
          (error (format "Cannot find file %s: Set `git-ps1-mode-ps1-file' properly"
                         git-ps1-mode-ps1-file)))
        (git-ps1-mode-update-current)
        (add-hook 'after-change-major-mode-hook
                  'git-ps1-mode-update-current)
        (add-hook 'after-save-hook
                  'git-ps1-mode-update-current)
        (add-hook 'window-configuration-change-hook
                  'git-ps1-mode-update-current)
        (setq git-ps1-mode-idle-timer-object
              (run-with-idle-timer git-ps1-mode-idle-interval
                                   t
                                   'git-ps1-mode-update-current)))
    (remove-hook 'after-change-major-mode-hook
                 'git-ps1-mode-update-current)
    (remove-hook 'after-save-hook
                 'git-ps1-mode-update-current)
    (remove-hook 'window-configuration-change-hook
                 'git-ps1-mode-update-current)
    (cancel-timer git-ps1-mode-idle-timer-object)
    (setq git-ps1-mode-idle-timer-object
          nil))
  (force-mode-line-update t))

(provide 'git-ps1-mode)

;;; git-ps1-mode.el ends here
