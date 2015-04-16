;;; git-ps1-mode.el --- Global minor-mode to print __git_ps1

;; Author: 10sr <8slashes+el [at] gmail [dot] com>
;; URL: https://github.com/10sr/git-ps1-mode-el
;; Version: 0.1.0
;; Keywords: utility mode-line git

;; Contributor: acple <silentsphere110@gmail.com>

;;; Code:

;; ===変数定義==================================================

;; モードラインに表示する文字列
;; Default値はプロセス実行前の表示
(defvar modeline-git-branch-string "[......]")

;; バッファ内Processオブジェクト
;; non-nilのときは新しいプロセスを実行しない
(defvar modeline-git-branch-process nil)

;; make local-variable
(make-variable-buffer-local 'modeline-git-branch-string)
(make-variable-buffer-local 'modeline-git-branch-process)


;; ===関数定義==================================================

;; プロセスの実行を予約する
(defun modeline-git-branch-schedule-update (buffer &optional force)
  (run-with-idle-timer
   0.0 nil #'modeline-git-branch-run-process buffer force))

;; プロセスを立ち上げてブランチ名を取得する
(defun modeline-git-branch-run-process (buffer force)
  (when (or (and force (buffer-live-p buffer))
            (eq buffer (current-buffer)))
    (with-current-buffer buffer
      (unless modeline-git-branch-process
        (let ((process-connection-type nil))
          (setq modeline-git-branch-process
                (start-process "modeline-git-branch" buffer
                               "git" "symbolic-ref" "HEAD"))
          (set-process-filter modeline-git-branch-process
                              'modeline-git-branch-update-modeline)
          (set-process-sentinel modeline-git-branch-process
                                'modeline-git-branch-clear-process)
          (set-process-query-on-exit-flag modeline-git-branch-process
                                          nil))))))

;; プロセスからの出力を整形してモードラインを更新する
(defun modeline-git-branch-update-modeline (process output)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (cond ((string= "fatal: ref HEAD is not a symbolic ref"
                      (substring output 0 -1))
             (setq modeline-git-branch-string "[no-branch]"))
            ((string-match "^fatal" output)
             (setq modeline-git-branch-string "[no-repo]"))
            (t
             (setq modeline-git-branch-string
                   (format "[%s]" (substring output 11 -1)))))
      (force-mode-line-update))))

;; 終了したプロセスをクリアする
(defun modeline-git-branch-clear-process (process state)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (setq modeline-git-branch-process nil))))


;; ===hook用関数定義============================================

;; hook#1 after-change-major-mode-hook, after-save-hook
(defun modeline-git-branch-update-current ()
  (unless (minibufferp (current-buffer))
    (modeline-git-branch-schedule-update (current-buffer) t)))

;; hook#2 select-window-functions
(defun modeline-git-branch-update-when-select-window
  (before-win after-win)
  (unless (minibufferp (window-buffer after-win))
    (modeline-git-branch-schedule-update (window-buffer after-win))))

;; hook#3 set-selected-window-buffer-functions
(defun modeline-git-branch-update-when-set-window-buffer
  (before-buf win after-buf)
  (unless (minibufferp after-buf)
    (modeline-git-branch-schedule-update after-buf)))


;; ===マイナーモード定義========================================

(defun modeline-git-branch-enable ()
  (setcar (or (member '(vc-mode vc-mode) mode-line-format)
              (list nil))
          'modeline-git-branch-string)
  (modeline-git-branch-update-current)
  (add-hook 'after-change-major-mode-hook
            'modeline-git-branch-update-current)
  (add-hook 'after-save-hook
            'modeline-git-branch-update-current)
  (add-hook 'select-window-functions
            'modeline-git-branch-update-when-select-window)
  (add-hook 'set-selected-window-buffer-functions
            'modeline-git-branch-update-when-set-window-buffer))

(defun modeline-git-branch-disable ()
  (setcar (or (memq 'modeline-git-branch-string mode-line-format)
              (list nil))
          '(vc-mode vc-mode))
  (remove-hook 'after-change-major-mode-hook
               'modeline-git-branch-update-current)
  (remove-hook 'after-save-hook
               'modeline-git-branch-update-current)
  (remove-hook 'select-window-functions
               'modeline-git-branch-update-when-select-window)
  (remove-hook 'set-selected-window-buffer-functions
               'modeline-git-branch-update-when-set-window-buffer))

(define-minor-mode modeline-git-branch-mode
  "[git-branch]"
  :group 'modeline-git-branch
  :global t
  (if modeline-git-branch-mode
      (modeline-git-branch-enable)
    (modeline-git-branch-disable))
  (force-mode-line-update t))

(provide 'git-ps1-mode)

;;; git-ps1-mode.el ends here
